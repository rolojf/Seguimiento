module BoardStore exposing (apply, timestamp, validDate, validOffset, validTimestamp, validateOpportunity, validatePending, validateProcess)

import Dict exposing (Dict)
import Domain exposing (..)
import Time
import Url


titleLimit : Int
titleLimit =
    90


textLimit : Int
textLimit =
    2000


contextLimit : Int
contextLimit =
    240


reasonLimit : Int
reasonLimit =
    600


offsetLimit : Float
offsetLimit =
    0.32


type alias Offset =
    { dx : Float, dy : Float }


cornerOffset : Float
cornerOffset =
    0.3


initialVersion : Int
initialVersion =
    1


templateVersion : Int
templateVersion =
    1



-- Every accepted command advances one board revision; rejected commands change nothing.


apply : String -> Change -> Board -> Result String Board
apply now change board =
    if not (validTimestamp now) then
        Err "La fecha del servidor no es válida."

    else
        changeBoard now change board
            |> Result.map (\updated -> { updated | revision = board.revision + 1 })


changeBoard : String -> Change -> Board -> Result String Board
changeBoard now change board =
    case change of
        SaveOpportunity target fields ->
            validateOpportunity fields
                |> Result.andThen (\_ -> saveOpportunity now target fields board)

        MoveOpportunity reference placement ->
            if validOffset placement.dx && validOffset placement.dy then
                changeOpportunity now
                    reference
                    (\item ->
                        let
                            fields =
                                item.fields
                        in
                        { item | fields = { fields | x = placement.x, y = placement.y }, dx = placement.dx, dy = placement.dy }
                    )
                    board

            else
                Err "El desplazamiento debe estar entre -0.32 y 0.32."

        ValidateOpportunity reference ->
            changeOpportunity now
                reference
                (\item ->
                    let
                        fields =
                            item.fields
                    in
                    { item | fields = { fields | proposed = False } }
                )
                board

        RemoveOpportunity reference ->
            changeOpportunity now reference (\item -> { item | deleted = True }) board

        RestoreOpportunity reference ->
            current reference board.opportunities
                |> Result.andThen
                    (\item ->
                        if item.deleted then
                            Ok { board | opportunities = Dict.insert item.id (touch now { item | deleted = False }) board.opportunities }

                        else
                            Err "El registro ya está activo."
                    )

        SavePending target fields ->
            validatePending fields
                |> Result.andThen (\_ -> savePending now target fields board)

        CompletePending reference completion ->
            changePending now
                reference
                (\item ->
                    let
                        fields =
                            item.fields
                    in
                    { item | fields = { fields | completion = completion } }
                )
                board

        RemovePending reference ->
            changePending now reference (\item -> { item | deleted = True }) board

        CreateProcess fields ->
            validateProcess fields
                |> Result.map (\_ -> createProcess now fields board)

        ChangeTask reference taskId changeTask ->
            changeProcess now reference taskId changeTask board


saveOpportunity : String -> Target -> OpportunityFields -> Board -> Result String Board
saveOpportunity now target fields board =
    case target of
        Existing reference ->
            changeOpportunity now
                reference
                (\item ->
                    let
                        offset =
                            if item.fields.x == fields.x && item.fields.y == fields.y then
                                Offset item.dx item.dy

                            else
                                nextOffset fields (Just item.id) board
                    in
                    { item | fields = fields, dx = offset.dx, dy = offset.dy }
                )
                board

        New ->
            let
                ( id, nextId ) =
                    freshId board

                offset =
                    nextOffset fields Nothing board

                item =
                    Opportunity id fields offset.dx offset.dy False initialVersion now now
            in
            Ok { board | opportunities = Dict.insert id item board.opportunities, nextId = nextId }


savePending : String -> Target -> PendingFields -> Board -> Result String Board
savePending now target fields board =
    case target of
        Existing reference ->
            changePending now reference (\item -> { item | fields = fields }) board

        New ->
            let
                ( id, nextId ) =
                    freshId board

                item =
                    Pending id fields False initialVersion now now
            in
            Ok { board | pending = Dict.insert id item board.pending, nextId = nextId }


createProcess : String -> ProcessFields -> Board -> Board
createProcess now fields board =
    let
        ( id, nextId ) =
            freshId board

        states =
            tasks fields.kind
                |> List.map (\task -> ( task.id, TaskState (Applicable Todo) Nothing ))
                |> Dict.fromList

        item =
            Process id { fields | client = String.trim fields.client } states Open "" templateVersion initialVersion now now
    in
    { board | processes = Dict.insert id item board.processes, nextId = nextId }



-- Match the original chart's repeatable separation inside an occupied rating cell.


nextOffset : OpportunityFields -> Maybe String -> Board -> Offset
nextOffset fields excluding board =
    let
        occupied =
            Dict.values board.opportunities
                |> List.filter (\item -> not item.deleted && item.fields.x == fields.x && item.fields.y == fields.y && Just item.id /= excluding)

        first =
            Offset (negate cornerOffset) (negate cornerOffset)

        candidates =
            [ Offset cornerOffset cornerOffset
            , Offset (negate cornerOffset) cornerOffset
            , Offset cornerOffset (negate cornerOffset)
            , Offset 0 (negate offsetLimit)
            , Offset 0 offsetLimit
            , Offset (negate offsetLimit) 0
            , Offset offsetLimit 0
            , Offset 0 0
            ]
    in
    if List.isEmpty occupied then
        Offset 0 0

    else
        List.foldl
            (\candidate best ->
                if nearestDistance occupied candidate > nearestDistance occupied best then
                    candidate

                else
                    best
            )
            first
            candidates


nearestDistance : List Opportunity -> Offset -> Float
nearestDistance occupied point =
    let
        distance item =
            (item.dx - point.dx) ^ 2 + (item.dy - point.dy) ^ 2
    in
    case occupied of
        [] ->
            0

        first :: rest ->
            List.foldl (\item closest -> min closest (distance item)) (distance first) rest


changeOpportunity : String -> Ref -> (Opportunity -> Opportunity) -> Board -> Result String Board
changeOpportunity now reference update board =
    active reference board.opportunities
        |> Result.map
            (\item -> { board | opportunities = Dict.insert item.id (touch now (update item)) board.opportunities })


changePending : String -> Ref -> (Pending -> Pending) -> Board -> Result String Board
changePending now reference update board =
    active reference board.pending
        |> Result.map
            (\item -> { board | pending = Dict.insert item.id (touch now (update item)) board.pending })


changeProcess : String -> Ref -> String -> TaskChange -> Board -> Result String Board
changeProcess now reference taskId change board =
    current reference board.processes
        |> Result.andThen
            (\process ->
                case ( List.filter (\task -> task.id == taskId) (tasks process.fields.kind), Dict.get taskId process.states ) of
                    ( [ task ], Just state ) ->
                        updateTask task change state
                            |> Result.map
                                (\updated ->
                                    { board | processes = Dict.insert process.id (touch now { process | states = Dict.insert taskId updated process.states }) board.processes }
                                )

                    _ ->
                        Err "El paso no pertenece a la plantilla del proceso."
            )



-- Evidence and personal closure remain independent of task completion.


updateTask : Task -> TaskChange -> TaskState -> Result String TaskState
updateTask task change state =
    case change of
        SetCompletion completion ->
            Ok { state | applicability = Applicable completion }

        ExcludeTask reason ->
            if not task.allowNotApplicable then
                Err "Este paso no permite marcar No aplica."

            else if String.length (String.trim reason) > reasonLimit then
                Err "El motivo de No aplica admite hasta 600 caracteres."

            else
                let
                    previous =
                        case state.applicability of
                            Applicable completion ->
                                completion

                            NotApplicable completion _ ->
                                completion
                in
                Ok { state | applicability = NotApplicable previous (String.trim reason) }

        RestoreTask ->
            case state.applicability of
                Applicable _ ->
                    Ok state

                NotApplicable previous _ ->
                    Ok { state | applicability = Applicable previous }


current : Ref -> Dict String { a | id : String, version : Int } -> Result String { a | id : String, version : Int }
current reference items =
    case Dict.get reference.id items of
        Nothing ->
            Err "El registro ya no está disponible."

        Just item ->
            if item.version == reference.version then
                Ok item

            else
                Err "El registro cambió en otro dispositivo. Revisa la versión actual antes de aplicar tu cambio."


active : Ref -> Dict String { a | id : String, version : Int, deleted : Bool } -> Result String { a | id : String, version : Int, deleted : Bool }
active reference items =
    current reference items
        |> Result.andThen
            (\item ->
                if item.deleted then
                    Err "El registro está eliminado."

                else
                    Ok item
            )


touch : String -> { a | version : Int, updatedAt : String } -> { a | version : Int, updatedAt : String }
touch now item =
    { item | version = item.version + 1, updatedAt = now }



-- One sequence covers all three tables, including imported and deleted records.


freshId : Board -> ( String, Int )
freshId board =
    seekId board (max 1 board.nextId)


seekId : Board -> Int -> ( String, Int )
seekId board candidate =
    let
        id =
            "elm-" ++ String.fromInt candidate
    in
    if Dict.member id board.opportunities || Dict.member id board.pending || Dict.member id board.processes then
        seekId board (candidate + 1)

    else
        ( id, candidate + 1 )


validateOpportunity : OpportunityFields -> Result String ()
validateOpportunity fields =
    if not (validTitle fields.title) then
        Err "El título es obligatorio y admite hasta 90 caracteres."

    else if String.length fields.notes > textLimit || String.length fields.source > textLimit then
        Err "Las notas y la fuente admiten hasta 2000 caracteres cada una."

    else if not (validSource fields.source) then
        Err "La fuente debe ser una dirección HTTP o HTTPS."

    else
        Ok ()


validatePending : PendingFields -> Result String ()
validatePending fields =
    if validTitle fields.title then
        Ok ()

    else
        Err "El pendiente es obligatorio y admite hasta 90 caracteres."


validateProcess : ProcessFields -> Result String ()
validateProcess fields =
    if not (validTitle fields.client) then
        Err "El cliente es obligatorio y admite hasta 90 caracteres."

    else if not (validDate fields.startDate) then
        Err "La fecha de inicio debe ser una fecha real con formato AAAA-MM-DD."

    else if String.length fields.context > contextLimit then
        Err "El contexto admite hasta 240 caracteres."

    else
        Ok ()


validTitle : String -> Bool
validTitle value =
    not (String.isEmpty (String.trim value)) && String.length value <= titleLimit


validSource : String -> Bool
validSource value =
    String.isEmpty value
        || ((String.startsWith "https://" value || String.startsWith "http://" value)
                && (Url.fromString value |> Maybe.map (\url -> not (String.isEmpty url.host)) |> Maybe.withDefault False)
           )


validOffset : Float -> Bool
validOffset value =
    not (isNaN value || isInfinite value) && abs value <= offsetLimit


validDate : String -> Bool
validDate value =
    case String.split "-" value of
        [ yearText, monthText, dayText ] ->
            case ( String.toInt yearText, String.toInt monthText, String.toInt dayText ) of
                ( Just year, Just month, Just day ) ->
                    value
                        == padded 4 year
                        ++ "-"
                        ++ padded 2 month
                        ++ "-"
                        ++ padded 2 day
                        && year
                        >= 0
                        && year
                        <= 9999
                        && month
                        >= 1
                        && month
                        <= 12
                        && day
                        >= 1
                        && day
                        <= daysInMonth year month

                _ ->
                    False

        _ ->
            False


daysInMonth : Int -> Int -> Int
daysInMonth year month =
    if month == 2 then
        if modBy 400 year == 0 || (modBy 4 year == 0 && modBy 100 year /= 0) then
            29

        else
            28

    else if List.member month [ 4, 6, 9, 11 ] then
        30

    else
        31


validTimestamp : String -> Bool
validTimestamp value =
    String.length value
        == 24
        && validDate (String.left 10 value)
        && String.slice 10 11 value
        == "T"
        && String.slice 13 14 value
        == ":"
        && String.slice 16 17 value
        == ":"
        && String.slice 19 20 value
        == "."
        && String.right 1 value
        == "Z"
        && timePart 23 (String.slice 11 13 value)
        && timePart 59 (String.slice 14 16 value)
        && timePart 59 (String.slice 17 19 value)
        && timePart 999 (String.slice 20 23 value)


timePart : Int -> String -> Bool
timePart maximum value =
    case String.toInt value of
        Just number ->
            number >= 0 && number <= maximum && value == padded (String.length value) number

        Nothing ->
            False


timestamp : Time.Posix -> String
timestamp value =
    padded 4 (Time.toYear Time.utc value)
        ++ "-"
        ++ padded 2 (monthNumber (Time.toMonth Time.utc value))
        ++ "-"
        ++ padded 2 (Time.toDay Time.utc value)
        ++ "T"
        ++ padded 2 (Time.toHour Time.utc value)
        ++ ":"
        ++ padded 2 (Time.toMinute Time.utc value)
        ++ ":"
        ++ padded 2 (Time.toSecond Time.utc value)
        ++ "."
        ++ padded 3 (modBy 1000 (Time.posixToMillis value))
        ++ "Z"


padded : Int -> Int -> String
padded width value =
    String.padLeft width '0' (String.fromInt value)


monthNumber : Time.Month -> Int
monthNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12
