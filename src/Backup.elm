module Backup exposing (decode, encode)

import BoardStore
import Dict exposing (Dict)
import Domain exposing (..)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Set exposing (Set)


formatName : String
formatName =
    "seguimiento-d1-backup"


formatVersion : Int
formatVersion =
    1


templateVersion : Int
templateVersion =
    1


snapshotVersion : Int
snapshotVersion =
    1


reasonLimit : Int
reasonLimit =
    600


decode : String -> Result String ( Board, Archive )
decode source =
    D.decodeString backupDecoder source |> Result.mapError decodeError


backupDecoder : Decoder ( Board, Archive )
backupDecoder =
    strict [ "format", "formatVersion", "exportedAt", "tables" ]
        []
        (D.succeed (\_ _ _ tables -> tables)
            |> using (D.field "format" (exactString formatName))
            |> using (D.field "formatVersion" (exactInt formatVersion))
            |> using (D.field "exportedAt" timestampDecoder)
            |> using (D.field "tables" tablesDecoder)
        )


tablesDecoder : Decoder ( Board, Archive )
tablesDecoder =
    strict [ "checks", "opportunities", "pending", "processes", "process_snapshots" ]
        []
        (D.map5
            (\checks opportunities pending processes snapshots ->
                let
                    ids =
                        Set.fromList (Dict.keys opportunities ++ Dict.keys pending ++ Dict.keys processes)
                in
                ( { opportunities = opportunities
                  , pending = pending
                  , processes = processes
                  , nextId = nextSequence ids 1
                  , revision = 0
                  }
                , Archive checks snapshots
                )
            )
            (D.field "checks" (archiveRows checkDecoder))
            (D.field "opportunities" (rows opportunityDecoder))
            (D.field "pending" (rows pendingDecoder))
            (D.field "processes" (rows processDecoder))
            (D.field "process_snapshots" (archiveRows snapshotDecoder))
        )


opportunityDecoder : Decoder Opportunity
opportunityDecoder =
    strict
        [ "id", "title", "category", "kind", "x", "y", "z", "dx", "dy", "notes", "source", "proposed", "version", "deleted", "created_at", "updated_at" ]
        []
        (D.map8 Opportunity
            (D.field "id" identifier)
            opportunityFields
            (D.field "dx" offsetDecoder)
            (D.field "dy" offsetDecoder)
            (D.field "deleted" sqliteBool)
            (D.field "version" positiveInt)
            (D.field "created_at" timestampDecoder)
            (D.field "updated_at" timestampDecoder)
        )


opportunityFields : Decoder OpportunityFields
opportunityFields =
    D.succeed OpportunityFields
        |> using (D.field "title" D.string)
        |> using (D.field "category" categoryDecoder)
        |> using (D.field "kind" kindDecoder)
        |> using (D.field "x" ratingDecoder)
        |> using (D.field "y" ratingDecoder)
        |> using (D.field "z" ratingDecoder)
        |> using (D.field "notes" D.string)
        |> using (D.field "source" D.string)
        |> using (D.field "proposed" sqliteBool)
        |> verified BoardStore.validateOpportunity


pendingDecoder : Decoder Pending
pendingDecoder =
    strict [ "id", "title", "owner", "done", "version", "deleted", "created_at", "updated_at" ]
        []
        (D.map6 Pending
            (D.field "id" identifier)
            (D.map3 PendingFields
                (D.field "title" D.string)
                (D.field "owner" ownerDecoder)
                (D.field "done"
                    (D.map
                        (\done ->
                            if done then
                                Done

                            else
                                Todo
                        )
                        sqliteBool
                    )
                )
                |> verified BoardStore.validatePending
            )
            (D.field "deleted" sqliteBool)
            (D.field "version" positiveInt)
            (D.field "created_at" timestampDecoder)
            (D.field "updated_at" timestampDecoder)
        )


processDecoder : Decoder Process
processDecoder =
    strict
        [ "id", "client", "type", "start_date", "context", "template_version", "states", "closed_override", "closed_reason", "legacy_summary", "version", "created_at", "updated_at" ]
        []
        (D.field "type" processKind
            |> D.andThen
                (\kind ->
                    D.succeed Process
                        |> using (D.field "id" identifier)
                        |> using
                            (D.map4 ProcessFields
                                (D.field "client" D.string)
                                (D.succeed kind)
                                (D.field "start_date" D.string)
                                (D.field "context" D.string)
                                |> verified BoardStore.validateProcess
                            )
                        |> using (D.field "states" (jsonString (D.dict taskDecoder |> verified (validateStates kind))))
                        |> using closureDecoder
                        |> using (D.field "legacy_summary" D.string)
                        |> using (D.field "template_version" (exactInt templateVersion))
                        |> using (D.field "version" positiveInt)
                        |> using (D.field "created_at" timestampDecoder)
                        |> using (D.field "updated_at" timestampDecoder)
                )
        )


closureDecoder : Decoder Closure
closureDecoder =
    D.map2 Tuple.pair (D.field "closed_override" sqliteBool) (D.field "closed_reason" D.string)
        |> D.andThen
            (\( closed, reason ) ->
                if closed then
                    D.succeed (PersonallyClosed reason)

                else if String.isEmpty reason then
                    D.succeed Open

                else
                    D.fail "Un proceso abierto no puede contener un motivo de cierre."
            )


taskDecoder : Decoder TaskState
taskDecoder =
    strict [ "status", "reason" ]
        [ "evidence", "priorStatus" ]
        (D.field "status" D.string
            |> D.andThen
                (\status ->
                    if status == "not_applicable" then
                        D.map3
                            (\previous reason evidence -> TaskState (NotApplicable previous reason) evidence)
                            (D.field "priorStatus" completionDecoder)
                            (D.field "reason" reasonDecoder)
                            (optional "evidence" D.string)

                    else
                        completionValue status
                            |> D.andThen
                                (\completion ->
                                    -- The old API can leave a redundant priorStatus after restoring a task.
                                    D.map3 (\_ _ evidence -> TaskState (Applicable completion) evidence)
                                        (D.field "reason" (exactString ""))
                                        (optional "priorStatus" completionDecoder)
                                        (optional "evidence" D.string)
                                )
                )
        )


validateStates : ProcessKind -> Dict String TaskState -> Result String ()
validateStates kind states =
    let
        template =
            tasks kind

        expected =
            List.map .id template |> List.sort

        unsupported task =
            case Dict.get task.id states |> Maybe.map .applicability of
                Just (NotApplicable _ _) ->
                    not task.allowNotApplicable

                _ ->
                    False
    in
    if Dict.keys states /= expected then
        Err "Los estados deben contener exactamente los pasos de la plantilla."

    else if List.any unsupported template then
        Err "Un paso de esta plantilla no permite No aplica."

    else
        Ok ()


checkDecoder : Decoder { id : String }
checkDecoder =
    strict [ "id", "done", "updated_at" ]
        []
        (D.map3 (\id _ _ -> { id = id })
            (D.field "id" identifier)
            (D.field "done" sqliteBool)
            (D.field "updated_at" timestampDecoder)
        )


snapshotDecoder : Decoder { id : String }
snapshotDecoder =
    strict [ "id", "schema_version", "payload", "created_at" ]
        []
        (D.map4 (\id _ _ _ -> { id = id })
            (D.field "id" identifier)
            (D.field "schema_version" (exactInt snapshotVersion))
            (D.field "payload" (jsonString snapshotPayload))
            (D.field "created_at" timestampDecoder)
        )


snapshotPayload : Decoder ()
snapshotPayload =
    strict
        [ "migrationId", "sourceSiteVersion", "sourceCommit", "createdAt", "oldDefaults", "effectiveChecks", "checks", "opportunities", "pending" ]
        []
        (D.succeed (\_ _ _ _ _ _ _ _ _ -> ())
            |> using (D.field "migrationId" identifier)
            |> using (D.field "sourceSiteVersion" positiveInt)
            |> using (D.field "sourceCommit" identifier)
            |> using (D.field "createdAt" timestampDecoder)
            |> using (D.field "oldDefaults" checkMap)
            |> using (D.field "effectiveChecks" checkMap)
            |> using (D.field "checks" (rows checkDecoder))
            |> using (D.field "opportunities" (rows opportunityDecoder))
            |> using (D.field "pending" (rows pendingDecoder))
        )


checkMap : Decoder (Dict String Bool)
checkMap =
    D.dict D.bool
        |> verified
            (\checks ->
                if List.any (String.trim >> String.isEmpty) (Dict.keys checks) then
                    Err "Las casillas históricas necesitan identificadores no vacíos."

                else
                    Ok ()
            )



-- Validate historical rows, then retain their original JSON values rather than discard fields.


archiveRows : Decoder { a | id : String } -> Decoder String
archiveRows decoder =
    D.map2 (\_ value -> E.encode 0 value) (rows decoder) D.value


rows : Decoder { a | id : String } -> Decoder (Dict String { a | id : String })
rows decoder =
    D.list decoder
        |> D.andThen
            (\items ->
                let
                    indexed =
                        Dict.fromList (List.map (\item -> ( item.id, item )) items)
                in
                if Dict.size indexed == List.length items then
                    D.succeed indexed

                else
                    D.fail "La tabla contiene identificadores duplicados."
            )


strict : List String -> List String -> Decoder a -> Decoder a
strict required allowed decoder =
    D.keyValuePairs D.value
        |> D.andThen
            (\pairs ->
                let
                    keys =
                        List.map Tuple.first pairs

                    missing =
                        List.filter (\key -> not (List.member key keys)) required

                    extra =
                        List.filter (\key -> not (List.member key (required ++ allowed))) keys
                in
                if List.isEmpty missing && List.isEmpty extra then
                    decoder

                else
                    D.fail ("Campos del respaldo incompatibles. Faltan: " ++ String.join ", " missing ++ ". No reconocidos: " ++ String.join ", " extra)
            )


optional : String -> Decoder a -> Decoder (Maybe a)
optional name decoder =
    D.keyValuePairs D.value
        |> D.andThen
            (\pairs ->
                if List.any (Tuple.first >> (==) name) pairs then
                    D.field name (D.map Just decoder)

                else
                    D.succeed Nothing
            )


using : Decoder a -> Decoder (a -> b) -> Decoder b
using argument function =
    D.map2 (\fn value -> fn value) function argument


verified : (a -> Result String ()) -> Decoder a -> Decoder a
verified validate decoder =
    decoder
        |> D.andThen
            (\value ->
                case validate value of
                    Ok () ->
                        D.succeed value

                    Err message ->
                        D.fail message
            )


jsonString : Decoder a -> Decoder a
jsonString decoder =
    D.string
        |> D.andThen
            (\source ->
                case D.decodeString decoder source of
                    Ok value ->
                        D.succeed value

                    Err error ->
                        D.fail (decodeError error)
            )


identifier : Decoder String
identifier =
    D.string
        |> verified
            (\value ->
                if String.isEmpty (String.trim value) then
                    Err "El identificador está vacío."

                else
                    Ok ()
            )


timestampDecoder : Decoder String
timestampDecoder =
    D.string
        |> verified
            (\value ->
                if BoardStore.validTimestamp value then
                    Ok ()

                else
                    Err "Se esperaba una fecha UTC válida con formato AAAA-MM-DDTHH:mm:ss.sssZ."
            )


positiveInt : Decoder Int
positiveInt =
    D.int
        |> verified
            (\value ->
                if value > 0 then
                    Ok ()

                else
                    Err "Se esperaba un entero positivo."
            )


exactInt : Int -> Decoder Int
exactInt expected =
    D.int
        |> verified
            (\value ->
                if value == expected then
                    Ok ()

                else
                    Err "Versión de formato o plantilla no compatible."
            )


exactString : String -> Decoder String
exactString expected =
    D.string
        |> verified
            (\value ->
                if value == expected then
                    Ok ()

                else
                    Err "Valor de formato o estado no compatible."
            )


sqliteBool : Decoder Bool
sqliteBool =
    D.int
        |> D.andThen
            (\value ->
                case value of
                    0 ->
                        D.succeed False

                    1 ->
                        D.succeed True

                    _ ->
                        D.fail "Se esperaba un booleano SQLite: 0 o 1."
            )


offsetDecoder : Decoder Float
offsetDecoder =
    D.string
        |> D.andThen
            (\value ->
                case String.toFloat value of
                    Just offset ->
                        if BoardStore.validOffset offset then
                            D.succeed offset

                        else
                            D.fail "Desplazamiento fuera del intervalo -0.32 a 0.32."

                    Nothing ->
                        D.fail "El desplazamiento debe ser un número guardado como texto."
            )


ratingDecoder : Decoder Rating
ratingDecoder =
    D.int
        |> D.andThen
            (\value ->
                case value of
                    1 ->
                        D.succeed One

                    2 ->
                        D.succeed Two

                    3 ->
                        D.succeed Three

                    4 ->
                        D.succeed Four

                    5 ->
                        D.succeed Five

                    _ ->
                        D.fail "Las valoraciones deben estar entre 1 y 5."
            )


categoryDecoder : Decoder Category
categoryDecoder =
    D.string
        |> D.andThen
            (\value ->
                case value of
                    "Ventex" ->
                        D.succeed Ventex

                    "Contratista" ->
                        D.succeed Contractor

                    _ ->
                        D.fail "Categoría no reconocida."
            )


kindDecoder : Decoder Kind
kindDecoder =
    D.string
        |> D.andThen
            (\value ->
                case value of
                    "opportunity" ->
                        D.succeed OpportunityKind

                    "responsibility" ->
                        D.succeed Responsibility

                    _ ->
                        D.fail "Tipo de oportunidad no reconocido."
            )


ownerDecoder : Decoder Owner
ownerDecoder =
    D.string
        |> D.andThen
            (\value ->
                case value of
                    "Rolando" ->
                        D.succeed Rolando

                    "dot" ->
                        D.succeed Dot

                    "Otros" ->
                        D.succeed Others

                    _ ->
                        D.fail "Responsable de pendiente no reconocido."
            )


processKind : Decoder ProcessKind
processKind =
    D.string
        |> D.andThen
            (\value ->
                case value of
                    "nueva_oti" ->
                        D.succeed NewOti

                    "entregas" ->
                        D.succeed Deliveries

                    _ ->
                        D.fail "Tipo de proceso no reconocido."
            )


completionDecoder : Decoder Completion
completionDecoder =
    D.string |> D.andThen completionValue


completionValue : String -> Decoder Completion
completionValue value =
    case value of
        "pending" ->
            D.succeed Todo

        "done" ->
            D.succeed Done

        _ ->
            D.fail "Estado de tarea no reconocido."


reasonDecoder : Decoder String
reasonDecoder =
    D.string
        |> verified
            (\value ->
                if String.length value <= reasonLimit then
                    Ok ()

                else
                    Err "El motivo de No aplica admite hasta 600 caracteres."
            )


nextSequence : Set String -> Int -> Int
nextSequence ids candidate =
    if Set.member ("elm-" ++ String.fromInt candidate) ids then
        nextSequence ids (candidate + 1)

    else
        candidate



-- Describe the failing path without echoing private record values in errors.


decodeError : D.Error -> String
decodeError error =
    case error of
        D.Field name inner ->
            name ++ ": " ++ decodeError inner

        D.Index index inner ->
            "fila " ++ String.fromInt (index + 1) ++ ": " ++ decodeError inner

        D.OneOf errors ->
            String.join "; " (List.map decodeError errors)

        D.Failure message _ ->
            message



-- Archive strings come only from the validated decoder or Domain.emptyArchive.
-- Keeping these fragments preserves the historical payload strings verbatim.


encode : String -> Board -> Archive -> String
encode exportedAt board archive =
    "{\"format\":"
        ++ E.encode 0 (E.string formatName)
        ++ ",\"formatVersion\":"
        ++ String.fromInt formatVersion
        ++ ",\"exportedAt\":"
        ++ E.encode 0 (E.string exportedAt)
        ++ ",\"tables\":{\"checks\":"
        ++ archive.checks
        ++ ",\"opportunities\":"
        ++ E.encode 0 (E.list encodeOpportunity (Dict.values board.opportunities))
        ++ ",\"pending\":"
        ++ E.encode 0 (E.list encodePending (Dict.values board.pending))
        ++ ",\"processes\":"
        ++ E.encode 0 (E.list encodeProcess (Dict.values board.processes))
        ++ ",\"process_snapshots\":"
        ++ archive.snapshots
        ++ "}}"


encodeOpportunity : Opportunity -> E.Value
encodeOpportunity item =
    E.object
        ([ ( "id", E.string item.id )
         , ( "title", E.string item.fields.title )
         , ( "category", E.string (categoryLabel item.fields.category) )
         , ( "kind"
           , E.string
                (if item.fields.kind == OpportunityKind then
                    "opportunity"

                 else
                    "responsibility"
                )
           )
         , ( "x", E.int (rating item.fields.x) )
         , ( "y", E.int (rating item.fields.y) )
         , ( "z", E.int (rating item.fields.z) )
         , ( "dx", E.string (String.fromFloat item.dx) )
         , ( "dy", E.string (String.fromFloat item.dy) )
         , ( "notes", E.string item.fields.notes )
         , ( "source", E.string item.fields.source )
         , ( "proposed", encodeBool item.fields.proposed )
         , ( "deleted", encodeBool item.deleted )
         ]
            ++ encodeMeta item
        )


encodePending : Pending -> E.Value
encodePending item =
    E.object
        ([ ( "id", E.string item.id )
         , ( "title", E.string item.fields.title )
         , ( "owner", E.string (ownerLabel item.fields.owner) )
         , ( "done", encodeBool (item.fields.completion == Done) )
         , ( "deleted", encodeBool item.deleted )
         ]
            ++ encodeMeta item
        )


encodeProcess : Process -> E.Value
encodeProcess item =
    let
        ( closed, reason ) =
            case item.closure of
                Open ->
                    ( False, "" )

                PersonallyClosed note ->
                    ( True, note )
    in
    E.object
        ([ ( "id", E.string item.id )
         , ( "client", E.string item.fields.client )
         , ( "type"
           , E.string
                (if item.fields.kind == NewOti then
                    "nueva_oti"

                 else
                    "entregas"
                )
           )
         , ( "start_date", E.string item.fields.startDate )
         , ( "context", E.string item.fields.context )
         , ( "template_version", E.int item.templateVersion )
         , ( "states", E.string (E.encode 0 (E.dict identity encodeTask item.states)) )
         , ( "closed_override", encodeBool closed )
         , ( "closed_reason", E.string reason )
         , ( "legacy_summary", E.string item.legacySummary )
         ]
            ++ encodeMeta item
        )


encodeTask : TaskState -> E.Value
encodeTask state =
    let
        fields =
            case state.applicability of
                Applicable completion ->
                    [ ( "status", E.string (completionName completion) ), ( "reason", E.string "" ) ]

                NotApplicable previous reason ->
                    [ ( "status", E.string "not_applicable" ), ( "reason", E.string reason ), ( "priorStatus", E.string (completionName previous) ) ]

        evidence =
            case state.evidence of
                Just value ->
                    [ ( "evidence", E.string value ) ]

                Nothing ->
                    []
    in
    E.object (fields ++ evidence)


completionName : Completion -> String
completionName completion =
    case completion of
        Todo ->
            "pending"

        Done ->
            "done"


encodeMeta : { a | version : Int, createdAt : String, updatedAt : String } -> List ( String, E.Value )
encodeMeta item =
    [ ( "version", E.int item.version ), ( "created_at", E.string item.createdAt ), ( "updated_at", E.string item.updatedAt ) ]


encodeBool : Bool -> E.Value
encodeBool value =
    E.int
        (if value then
            1

         else
            0
        )
