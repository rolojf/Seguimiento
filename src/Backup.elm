module Backup exposing (decode, encode)

import BoardStore
import Dict exposing (Dict)
import Domain exposing (..)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Set exposing (Set)


formatName : String
formatName =
    "seguimiento-lamdera-backup"


formatVersion : Int
formatVersion =
    4


singleOwnerVersion : Int
singleOwnerVersion =
    3


previousVersion : Int
previousVersion =
    2


legacyFormat : String
legacyFormat =
    "seguimiento-d1-backup"


legacyVersion : Int
legacyVersion =
    1


templateVersion : Int
templateVersion =
    1


snapshotVersion : Int
snapshotVersion =
    1


maxTaskNotes : Int
maxTaskNotes =
    10000


type RowFormat
    = LegacyRows
    | CurrentRows
    | MultiOwnerRows


decode : String -> Result String ( Board, Archive )
decode source =
    D.decodeString backupDecoder source |> Result.mapError decodeError


backupDecoder : Decoder ( Board, Archive )
backupDecoder =
    D.map2 Tuple.pair (D.field "format" D.string) (D.field "formatVersion" D.int)
        |> D.andThen
            (\( format, version ) ->
                if format == formatName && version == formatVersion then
                    currentBackup MultiOwnerRows (rows (pendingDecoder MultiOwnerRows pendingContent) |> verified validPendingOrder)

                else if format == formatName && version == singleOwnerVersion then
                    currentBackup CurrentRows (rows (pendingDecoder CurrentRows pendingContent) |> verified validPendingOrder)

                else if format == formatName && version == previousVersion then
                    currentBackup LegacyRows
                        (rows (pendingDecoder LegacyRows previousContent)
                            |> verified validPreviousOrder
                            |> D.map reindexPending
                        )

                else if format == legacyFormat && version == legacyVersion then
                    legacyBackup

                else
                    D.fail "Formato o versión de respaldo no compatible."
            )


legacyBackup : Decoder ( Board, Archive )
legacyBackup =
    strict [ "format", "formatVersion", "exportedAt", "tables" ]
        []
        (D.map2 (\_ tables -> tables)
            (D.field "exportedAt" timestampDecoder)
            (D.field "tables" (tablesDecoder LegacyRows (rows legacyPending |> D.map reindexPending)))
        )


currentBackup : RowFormat -> Decoder (Dict String Pending) -> Decoder ( Board, Archive )
currentBackup rowFormat pendingRows =
    strict [ "format", "formatVersion", "exportedAt", "nextId", "tables" ]
        []
        (D.map3 (\_ nextId tables -> ( nextId, tables ))
            (D.field "exportedAt" timestampDecoder)
            (D.field "nextId" positiveInt)
            (D.field "tables" (tablesDecoder rowFormat pendingRows))
            |> D.andThen
                (\( nextId, ( board, archive ) ) ->
                    if nextId >= board.nextId then
                        D.succeed ( { board | nextId = nextId }, archive )

                    else
                        D.fail "nextId debe superar los identificadores elm- existentes."
                )
        )


tablesDecoder : RowFormat -> Decoder (Dict String Pending) -> Decoder ( Board, Archive )
tablesDecoder rowFormat pendingRows =
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
                  , nextId = nextSequence ids
                  , revision = 0
                  , pendingOrderRevision = 0
                  }
                , Archive checks snapshots
                )
            )
            (D.field "checks" (archiveRows checkDecoder))
            (D.field "opportunities" (rows (opportunityDecoder rowFormat)))
            (D.field "pending" pendingRows)
            (D.field "processes" (rows (processDecoder rowFormat)))
            (D.field "process_snapshots" (archiveRows snapshotDecoder))
            |> verified
                (\( board, _ ) ->
                    if rowFormat == MultiOwnerRows then
                        validateLinks board

                    else
                        Ok ()
                )
        )


opportunityDecoder : RowFormat -> Decoder Opportunity
opportunityDecoder format =
    let
        extra =
            case format of
                LegacyRows ->
                    []

                _ ->
                    [ "status", "completionSuggested" ]
    in
    strict
        [ "id", "title", "category", "kind", "x", "y", "z", "dx", "dy", "notes", "source", "proposed", "version", "deleted", "created_at", "updated_at" ]
        extra
        (D.map8
            (\id fields dx dy status version createdAt updatedAt ->
                Opportunity id fields dx dy status version createdAt updatedAt Reviewed
            )
            (D.field "id" identifier)
            opportunityFields
            (D.field "dx" offsetDecoder)
            (D.field "dy" offsetDecoder)
            (opportunityState format)
            (D.field "version" positiveInt)
            (D.field "created_at" timestampDecoder)
            (D.field "updated_at" timestampDecoder)
        )


opportunityState : RowFormat -> Decoder OpportunityStatus
opportunityState format =
    let
        legacy =
            D.field "deleted" sqliteBool
                |> D.map
                    (\deleted ->
                        if deleted then
                            DiscardedOpportunity

                        else
                            ActiveOpportunity NoCompletionProposal
                    )
    in
    case format of
        LegacyRows ->
            legacy

        _ ->
            -- Older version 3 rows infer their state from deleted and have no proposal.
            D.map3
                (\old explicit suggested ->
                    let
                        status =
                            Maybe.withDefault old explicit
                    in
                    if (status == DiscardedOpportunity) /= (old == DiscardedOpportunity) then
                        D.fail "El estado de la oportunidad contradice deleted."

                    else if Maybe.withDefault False suggested then
                        case status of
                            ActiveOpportunity _ ->
                                D.succeed (ActiveOpportunity CompletionProposed)

                            _ ->
                                D.fail "Solo una oportunidad activa puede tener una propuesta de cumplimiento."

                    else
                        D.succeed status
                )
                legacy
                (optional "status" oppStatusDecoder)
                (optional "completionSuggested" D.bool)
                |> D.andThen identity


oppStatusDecoder : Decoder OpportunityStatus
oppStatusDecoder =
    D.string
        |> D.andThen
            (\status ->
                case status of
                    "active" ->
                        D.succeed (ActiveOpportunity NoCompletionProposal)

                    "discarded" ->
                        D.succeed DiscardedOpportunity

                    "completed" ->
                        D.succeed CompletedOpportunity

                    _ ->
                        D.fail "Estado de oportunidad no reconocido."
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


legacyPending : Decoder Pending
legacyPending =
    strict [ "id", "title", "owner", "done", "version", "deleted", "created_at", "updated_at" ]
        []
        (D.map6
            (\id fields deleted version createdAt updatedAt ->
                Pending id fields deleted version createdAt updatedAt 0 Standalone
            )
            (D.field "id" identifier)
            (D.map3 (\title owner status -> PendingFields (PendingTask title status) owner)
                (D.field "title" D.string)
                (D.field "owner" ownerDecoder)
                (D.field "done" oldPendingStatus)
                |> verified BoardStore.validatePending
            )
            (D.field "deleted" sqliteBool)
            (D.field "version" positiveInt)
            (D.field "created_at" timestampDecoder)
            (D.field "updated_at" timestampDecoder)
        )


pendingDecoder : RowFormat -> Decoder PendingContent -> Decoder Pending
pendingDecoder format contentDecoder =
    strict
        ([ "id", "content", "owner", "position", "version", "deleted", "created_at", "updated_at" ]
            ++ (if format == MultiOwnerRows then
                    [ "origin" ]

                else
                    []
               )
        )
        []
        (D.map8 Pending
            (D.field "id" identifier)
            (D.map2 PendingFields
                (D.field "content" contentDecoder)
                (D.field "owner" ownerDecoder)
                |> verified BoardStore.validatePending
            )
            (D.field "deleted" sqliteBool)
            (D.field "version" positiveInt)
            (D.field "created_at" timestampDecoder)
            (D.field "updated_at" timestampDecoder)
            (D.field "position" nonnegativeInt)
            (if format == MultiOwnerRows then
                D.field "origin" originDecoder

             else
                D.succeed Standalone
            )
        )


originDecoder : Decoder PendingOrigin
originDecoder =
    D.field "type" D.string
        |> D.andThen
            (\kind ->
                case kind of
                    "standalone" ->
                        strict [ "type" ] [] (D.succeed Standalone)

                    "processTask" ->
                        strict [ "type", "processId", "taskId" ]
                            []
                            (D.map2 ProcessTask
                                (D.field "processId" identifier)
                                (D.field "taskId" identifier)
                            )

                    _ ->
                        D.fail "Origen de pendiente no reconocido."
            )


pendingContent : Decoder PendingContent
pendingContent =
    contentWith "status" pendingStatus


previousContent : Decoder PendingContent
previousContent =
    contentWith "done" oldPendingStatus


contentWith : String -> Decoder PendingStatus -> Decoder PendingContent
contentWith statusField statusDecoder =
    D.field "type" D.string
        |> D.andThen
            (\kind ->
                case kind of
                    "task" ->
                        strict [ "type", "title", statusField ]
                            []
                            (D.map2 PendingTask
                                (D.field "title" D.string)
                                (D.field statusField statusDecoder)
                            )

                    "separator" ->
                        strict [ "type", "text" ]
                            []
                            (D.map Separator (D.field "text" D.string))

                    _ ->
                        D.fail "Tipo de fila de pendientes no reconocido."
            )


oldPendingStatus : Decoder PendingStatus
oldPendingStatus =
    D.map
        (\done ->
            if done then
                LegacyCompleted

            else
                Approved
        )
        sqliteBool


pendingStatus : Decoder PendingStatus
pendingStatus =
    D.field "type" D.string
        |> D.andThen
            (\status ->
                case status of
                    "awaitingApproval" ->
                        strict [ "type", "user" ]
                            []
                            (D.map AwaitingApproval (D.field "user" userDecoder))

                    "approved" ->
                        strict [ "type" ] [] (D.succeed Approved)

                    "completed" ->
                        strict [ "type", "by", "review" ]
                            []
                            (D.map2 Tuple.pair
                                (D.field "by" userDecoder)
                                (D.field "review" D.string)
                                |> D.andThen (\( by, review ) -> completedStatus by review)
                            )

                    "legacyCompleted" ->
                        strict [ "type" ] [] (D.succeed LegacyCompleted)

                    _ ->
                        D.fail "Estado de pendiente no reconocido."
            )


completedStatus : User -> String -> Decoder PendingStatus
completedStatus by review =
    case review of
        "reviewed" ->
            D.succeed (CompletedBy by Reviewed)

        "pending" ->
            -- The completer cannot also be the user asked to confirm completion.
            D.succeed
                (CompletedBy by
                    (NeedsReview
                        (case by of
                            UserOne ->
                                UserTwo

                            UserTwo ->
                                UserOne
                        )
                    )
                )

        _ ->
            D.fail "La revisión de cumplimiento debe ser reviewed o pending."


userDecoder : Decoder User
userDecoder =
    D.string
        |> D.andThen
            (\name ->
                case name of
                    "Rolo" ->
                        D.succeed UserOne

                    "Dot" ->
                        D.succeed UserTwo

                    _ ->
                        D.fail "El usuario debe ser Rolo o Dot."
            )



-- Keep the old relative order while completed rows leave the active list.
-- D1 positions start at zero, so its original created_at/id order is retained.


reindexPending : Dict String Pending -> Dict String Pending
reindexPending items =
    List.foldl
        (\owner result ->
            orderedPending owner { empty | pending = result }
                |> List.indexedMap (\position item -> { item | position = position })
                |> List.foldl (\item indexed -> Dict.insert item.id item indexed) result
        )
        items
        [ Rolando, Dot, Others ]


validPendingOrder : Dict String Pending -> Result String ()
validPendingOrder items =
    validateOrder (\owner -> orderedPending owner { empty | pending = items })


validPreviousOrder : Dict String Pending -> Result String ()
validPreviousOrder items =
    -- Version 2 positions included completed tasks among the active rows.
    validateOrder
        (\owner ->
            Dict.values items
                |> List.filter (\item -> not item.deleted && item.fields.owner == owner)
                |> List.sortBy .position
        )


validateOrder : (Owner -> List Pending) -> Result String ()
validateOrder ordered =
    let
        valid owner =
            let
                positions =
                    ordered owner |> List.map .position
            in
            positions == List.range 0 (List.length positions - 1)
    in
    if List.all valid [ Rolando, Dot, Others ] then
        Ok ()

    else
        Err "Las posiciones activas de cada responsable deben ser consecutivas, desde 0 y sin duplicados."


processDecoder : RowFormat -> Decoder Process
processDecoder format =
    let
        ( extra, statusDecoder ) =
            case format of
                LegacyRows ->
                    ( [], D.succeed ActiveProcess )

                CurrentRows ->
                    -- Earlier version 3 exports did not include a process status.
                    ( [ "status" ], optional "status" processStatus |> D.map (Maybe.withDefault ActiveProcess) )

                MultiOwnerRows ->
                    ( [ "status" ], D.field "status" processStatus )
    in
    strict
        [ "id", "client", "type", "start_date", "context", "template_version", "states", "closed_override", "closed_reason", "legacy_summary", "version", "created_at", "updated_at" ]
        extra
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
                        |> using statusDecoder
                        |> using (D.field "states" (jsonString (statesDecoder format kind)))
                        |> using closureDecoder
                        |> using (D.field "legacy_summary" D.string)
                        |> using (D.field "template_version" (exactInt templateVersion))
                        |> using (D.field "version" positiveInt)
                        |> using (D.field "created_at" timestampDecoder)
                        |> using (D.field "updated_at" timestampDecoder)
                )
        )


processStatus : Decoder ProcessStatus
processStatus =
    D.string
        |> D.andThen
            (\status ->
                case status of
                    "active" ->
                        D.succeed ActiveProcess

                    "completed" ->
                        D.succeed CompletedProcess

                    _ ->
                        D.fail "Estado de proceso no reconocido."
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


statesDecoder : RowFormat -> ProcessKind -> Decoder (Dict String TaskState)
statesDecoder format kind =
    let
        template =
            tasks kind
    in
    strict (List.map .id template)
        []
        (List.foldl
            (\task decoder ->
                D.map2 (Dict.insert task.id)
                    (D.field task.id (taskDecoder format task))
                    decoder
            )
            (D.succeed Dict.empty)
            template
        )
        |> verified (validateStates kind)


taskDecoder : RowFormat -> Task -> Decoder TaskState
taskDecoder format task =
    if format == MultiOwnerRows then
        strict [ "applicability", "completions", "notes" ]
            [ "evidence" ]
            (D.map4
                (\applicability completions evidence notes ->
                    TaskState applicability completions evidence notes Reviewed
                )
                (D.field "applicability" applicabilityDecoder)
                (D.field "completions" (D.list ownerStateDecoder))
                (optional "evidence" D.string)
                (D.field "notes" taskNotes)
            )

    else
        oldTaskDecoder format task


applicabilityDecoder : Decoder Applicability
applicabilityDecoder =
    D.string
        |> D.andThen
            (\value ->
                case value of
                    "applicable" ->
                        D.succeed Applicable

                    "not_applicable" ->
                        D.succeed NotApplicable

                    _ ->
                        D.fail "Aplicabilidad de tarea no reconocida."
            )


ownerStateDecoder : Decoder OwnerState
ownerStateDecoder =
    strict [ "owner", "completion" ]
        []
        (D.map2 (\owner completion -> OwnerState owner completion Reviewed)
            (D.field "owner" ownerDecoder)
            (D.field "completion" completionDecoder)
        )


oldTaskDecoder : RowFormat -> Task -> Decoder TaskState
oldTaskDecoder format task =
    let
        ( extra, notesDecoder ) =
            case format of
                LegacyRows ->
                    ( [], D.succeed "" )

                _ ->
                    ( [ "notes" ], optional "notes" taskNotes |> D.map (Maybe.withDefault "") )
    in
    strict [ "status" ]
        ([ "reason", "evidence", "priorStatus" ] ++ extra)
        (D.field "status" D.string
            |> D.andThen
                (\status ->
                    if status == "not_applicable" then
                        D.map4
                            (\previous _ evidence notes ->
                                TaskState NotApplicable (oldCompletions task previous) evidence notes Reviewed
                            )
                            (D.field "priorStatus" completionDecoder)
                            (optional "reason" D.string)
                            (optional "evidence" D.string)
                            notesDecoder

                    else
                        completionValue status
                            |> D.andThen
                                (\completion ->
                                    -- Legacy comments are deliberately discarded; reviews begin acknowledged.
                                    D.map4 (\_ _ evidence notes -> TaskState Applicable (oldCompletions task completion) evidence notes Reviewed)
                                        (optional "reason" D.string)
                                        (optional "priorStatus" completionDecoder)
                                        (optional "evidence" D.string)
                                        notesDecoder
                                )
                )
        )


oldCompletions : Task -> Completion -> List OwnerState
oldCompletions task completion =
    List.map (\owner -> OwnerState owner completion Reviewed) (taskOwners task)


taskNotes : Decoder String
taskNotes =
    D.string
        |> verified
            (\notes ->
                if String.length notes <= maxTaskNotes then
                    Ok ()

                else
                    Err ("Las notas de un paso no pueden superar " ++ String.fromInt maxTaskNotes ++ " caracteres.")
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
                Just NotApplicable ->
                    not task.allowNotApplicable

                _ ->
                    False
    in
    if Dict.keys states /= expected then
        Err "Los estados deben contener exactamente los pasos de la plantilla."

    else if List.any unsupported template then
        Err "Un paso de esta plantilla no permite No aplica."

    else if
        List.any
            (\task ->
                Dict.get task.id states
                    |> Maybe.map
                        (\state ->
                            let
                                owners =
                                    List.map .owner state.completions

                                assigned =
                                    taskOwners task
                            in
                            List.length owners
                                /= List.length assigned
                                || not (List.all (\owner -> List.member owner owners) assigned)
                        )
                    |> Maybe.withDefault True
            )
            template
    then
        Err "El cumplimiento debe contener exactamente una entrada por responsable asignado."

    else
        Ok ()


validateLinks : Board -> Result String ()
validateLinks board =
    Dict.values board.pending
        |> List.foldl
            (\item result ->
                result
                    |> Result.andThen
                        (\seen ->
                            case item.origin of
                                Standalone ->
                                    Ok seen

                                ProcessTask processId taskId ->
                                    let
                                        key =
                                            ( processId, taskId, item.fields.owner )
                                    in
                                    if List.member key seen then
                                        Err "Hay pendientes con el mismo vínculo de proceso, tarea y responsable."

                                    else
                                        validateLink board item processId taskId
                                            |> Result.map (\_ -> key :: seen)
                        )
            )
            (Ok [])
        |> Result.map (\_ -> ())


validateLink : Board -> Pending -> String -> String -> Result String ()
validateLink board item processId taskId =
    case item.fields.content of
        Separator _ ->
            Err "Un separador no puede estar vinculado a un proceso."

        PendingTask _ _ ->
            case Dict.get processId board.processes of
                Nothing ->
                    Err "El proceso vinculado al pendiente no existe."

                Just process ->
                    case List.filter (\task -> task.id == taskId) (tasks process.fields.kind) |> List.head of
                        Nothing ->
                            Err "La tarea vinculada al pendiente no existe."

                        Just task ->
                            let
                                state =
                                    taskState taskId process

                                complete =
                                    pendingIsComplete item.fields.content
                            in
                            if not (List.member item.fields.owner (taskOwners task)) then
                                Err "El responsable del pendiente no está asignado a la tarea vinculada."

                            else if item.deleted then
                                -- Discarded links retain their own history, not the current task completion.
                                Ok ()

                            else if not complete && state.applicability == NotApplicable then
                                Err "Una actividad cancelada no puede tener pendientes vinculados abiertos."

                            else if not complete && process.status == CompletedProcess then
                                Err "Un proceso completado no puede tener pendientes vinculados abiertos."

                            else if ((ownerState item.fields.owner state).completion == Done) /= complete then
                                Err "El cumplimiento de la tarea no coincide con el pendiente vinculado."

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
            |> using (D.field "opportunities" (rows (opportunityDecoder LegacyRows)))
            |> using (D.field "pending" (rows legacyPending))
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


nonnegativeInt : Decoder Int
nonnegativeInt =
    D.int
        |> verified
            (\value ->
                if value >= 0 then
                    Ok ()

                else
                    Err "La posición debe ser un entero no negativo."
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

                    "Rolo" ->
                        D.succeed Rolando

                    "dot" ->
                        D.succeed Dot

                    "Dot" ->
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


nextSequence : Set String -> Int
nextSequence ids =
    ids
        |> Set.toList
        |> List.filterMap sequenceNumber
        |> List.maximum
        |> Maybe.withDefault 0
        |> (+) 1


sequenceNumber : String -> Maybe Int
sequenceNumber id =
    if String.startsWith "elm-" id then
        String.dropLeft 4 id
            |> String.toInt
            |> Maybe.andThen
                (\number ->
                    if number > 0 && id == "elm-" ++ String.fromInt number then
                        Just number

                    else
                        Nothing
                )

    else
        Nothing



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
        ++ ",\"nextId\":"
        ++ String.fromInt board.nextId
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
         , ( "deleted", encodeBool (opportunityDiscarded item) )
         , ( "status", E.string (oppStatusName item.status) )
         , ( "completionSuggested", E.bool (completionSuggested item) )
         ]
            ++ encodeMeta item
        )


oppStatusName : OpportunityStatus -> String
oppStatusName status =
    case status of
        ActiveOpportunity _ ->
            "active"

        DiscardedOpportunity ->
            "discarded"

        CompletedOpportunity ->
            "completed"


encodePending : Pending -> E.Value
encodePending item =
    E.object
        ([ ( "id", E.string item.id )
         , ( "content", encodePendingContent item.fields.content )
         , ( "owner", E.string (ownerLabel item.fields.owner) )
         , ( "origin", encodeOrigin item.origin )
         , ( "position", E.int item.position )
         , ( "deleted", encodeBool item.deleted )
         ]
            ++ encodeMeta item
        )


encodeOrigin : PendingOrigin -> E.Value
encodeOrigin origin =
    case origin of
        Standalone ->
            E.object [ ( "type", E.string "standalone" ) ]

        ProcessTask processId taskId ->
            E.object
                [ ( "type", E.string "processTask" )
                , ( "processId", E.string processId )
                , ( "taskId", E.string taskId )
                ]


encodePendingContent : PendingContent -> E.Value
encodePendingContent content =
    case content of
        PendingTask title status ->
            E.object
                [ ( "type", E.string "task" )
                , ( "title", E.string title )
                , ( "status", encodePendingStatus status )
                ]

        Separator title ->
            E.object [ ( "type", E.string "separator" ), ( "text", E.string title ) ]


encodePendingStatus : PendingStatus -> E.Value
encodePendingStatus status =
    case status of
        AwaitingApproval reviewer ->
            E.object
                [ ( "type", E.string "awaitingApproval" )
                , ( "user", E.string (userName reviewer) )
                ]

        Approved ->
            E.object [ ( "type", E.string "approved" ) ]

        CompletedBy by review ->
            E.object
                [ ( "type", E.string "completed" )
                , ( "by", E.string (userName by) )
                , ( "review"
                  , E.string
                        (case review of
                            Reviewed ->
                                "reviewed"

                            NeedsReview _ ->
                                "pending"
                        )
                  )
                ]

        LegacyCompleted ->
            E.object [ ( "type", E.string "legacyCompleted" ) ]


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
         , ( "status", E.string (processStatusName item.status) )
         , ( "template_version", E.int item.templateVersion )
         , ( "states", E.string (E.encode 0 (E.dict identity encodeTask item.states)) )
         , ( "closed_override", encodeBool closed )
         , ( "closed_reason", E.string reason )
         , ( "legacy_summary", E.string item.legacySummary )
         ]
            ++ encodeMeta item
        )


processStatusName : ProcessStatus -> String
processStatusName status =
    case status of
        ActiveProcess ->
            "active"

        CompletedProcess ->
            "completed"


encodeTask : TaskState -> E.Value
encodeTask state =
    let
        fields =
            [ ( "applicability"
              , E.string
                    (case state.applicability of
                        Applicable ->
                            "applicable"

                        NotApplicable ->
                            "not_applicable"
                    )
              )
            , ( "completions", E.list encodeOwnerState state.completions )
            ]

        evidence =
            case state.evidence of
                Just value ->
                    [ ( "evidence", E.string value ) ]

                Nothing ->
                    []
    in
    E.object (fields ++ evidence ++ [ ( "notes", E.string state.notes ) ])


encodeOwnerState : OwnerState -> E.Value
encodeOwnerState state =
    E.object
        [ ( "owner", E.string (ownerLabel state.owner) )
        , ( "completion", E.string (completionName state.completion) )
        ]


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
