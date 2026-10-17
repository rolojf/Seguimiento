module Backend exposing (app)

import Access
import Backup
import BoardStore
import Dict
import Domain exposing (User)
import Lamdera exposing (ClientId, SessionId)
import Task
import Time
import Types exposing (BackendModel, BackendMsg(..), PrivateData, Session, ToBackend(..), ToFrontend(..))


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.batch [ Lamdera.onConnect OnConnect, Lamdera.onDisconnect OnDisconnect ]
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { board = Domain.empty, archive = Domain.emptyArchive, sessions = Dict.empty, clients = Dict.empty }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        OnConnect sid cid ->
            ( { model | clients = Dict.insert cid sid model.clients }
            , Lamdera.sendToFrontend cid (SessionStatus (snapshot sid model))
            )

        OnDisconnect _ cid ->
            ( { model | clients = Dict.remove cid model.clients }, Cmd.none )

        ApplyChange sid cid session request change now ->
            if sameSession sid session model then
                case BoardStore.apply session.user (BoardStore.timestamp now) change model.board of
                    Ok board ->
                        accept cid request { model | board = board }

                    Err error ->
                        reject cid request error model

            else
                denied cid model

        ApplyImport sid cid session request source ->
            if not (sameSession sid session model) then
                denied cid model

            else if not (isEmpty model) then
                reject cid request "El tablero ya contiene datos o historial. La importación solo se permite sobre un tablero vacío." model

            else
                case Backup.decode source of
                    Ok ( board, archive ) ->
                        let
                            -- Keep client revisions and IDs monotonic even after all rows were purged.
                            restored =
                                { board
                                    | revision = model.board.revision + 1
                                    , pendingOrderRevision = model.board.pendingOrderRevision + 1
                                    , nextId = max model.board.nextId board.nextId
                                }
                        in
                        accept cid request { model | board = restored, archive = archive }

                    Err error ->
                        reject cid request error model

        PrepareExport sid cid session request now ->
            if sameSession sid session model then
                ( model, Lamdera.sendToFrontend cid (ExportReady request (Backup.encode (BoardStore.timestamp now) model.board model.archive)) )

            else
                denied cid model


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sid cid msg model =
    case msg of
        LogIn username key ->
            case Access.verify username key of
                Just session ->
                    let
                        updated =
                            { model | sessions = Dict.insert sid session model.sessions }
                    in
                    ( updated, sendSession sid (SessionStatus (snapshot sid updated)) updated )

                Nothing ->
                    ( model, Lamdera.sendToFrontend cid LoginRejected )

        LogOut ->
            ( { model | sessions = Dict.remove sid model.sessions }
            , sendSession sid (SessionStatus Nothing) model
            )

        _ ->
            -- Every board operation passes the same server-side access boundary.
            case Dict.get sid model.sessions of
                Just session ->
                    if Access.user session == Nothing then
                        denied cid model

                    else
                        authorized sid cid session msg model

                Nothing ->
                    denied cid model


authorized : SessionId -> ClientId -> Session -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
authorized sid cid session msg model =
    case msg of
        ChangeBoard request change ->
            ( model, Task.perform (ApplyChange sid cid session request change) Time.now )

        ImportBoard request source ->
            if String.length source > maxBackupSize then
                reject cid request "El respaldo supera el límite de 5 MB." model

            else
                -- Queue import like other writes and recheck the session at execution.
                ( model, Task.perform (\_ -> ApplyImport sid cid session request source) (Task.succeed ()) )

        ExportBoard request ->
            ( model, Task.perform (PrepareExport sid cid session request) Time.now )

        _ ->
            ( model, Cmd.none )


maxBackupSize : Int
maxBackupSize =
    5 * 1024 * 1024


sameSession : SessionId -> Session -> BackendModel -> Bool
sameSession sid session model =
    Dict.get sid model.sessions == Just session && Access.user session /= Nothing


sessionUser : SessionId -> BackendModel -> Maybe User
sessionUser sid model =
    Dict.get sid model.sessions |> Maybe.andThen Access.user


snapshot : SessionId -> BackendModel -> Maybe PrivateData
snapshot sid model =
    sessionUser sid model |> Maybe.map (\user -> { user = user, board = model.board })


isEmpty : BackendModel -> Bool
isEmpty model =
    Dict.isEmpty model.board.opportunities
        && Dict.isEmpty model.board.pending
        && Dict.isEmpty model.board.processes
        && model.archive
        == Domain.emptyArchive


accept : ClientId -> Int -> BackendModel -> ( BackendModel, Cmd BackendMsg )
accept cid request model =
    ( model
    , Cmd.batch
        [ sendPrivate (BoardChanged model.board) model
        , Lamdera.sendToFrontend cid (ChangeResult request (Ok model.board))
        ]
    )


reject : ClientId -> Int -> String -> BackendModel -> ( BackendModel, Cmd BackendMsg )
reject cid request error model =
    ( model
    , Cmd.batch
        [ Lamdera.sendToFrontend cid (BoardChanged model.board)
        , Lamdera.sendToFrontend cid (ChangeResult request (Err error))
        ]
    )


denied : ClientId -> BackendModel -> ( BackendModel, Cmd BackendMsg )
denied cid model =
    ( model, Lamdera.sendToFrontend cid (SessionStatus Nothing) )


sendSession : SessionId -> ToFrontend -> BackendModel -> Cmd BackendMsg
sendSession sid message model =
    model.clients
        |> Dict.toList
        |> List.filter (\( _, clientSession ) -> clientSession == sid)
        |> List.map (\( cid, _ ) -> Lamdera.sendToFrontend cid message)
        |> Cmd.batch


sendPrivate : ToFrontend -> BackendModel -> Cmd BackendMsg
sendPrivate message model =
    model.clients
        |> Dict.toList
        |> List.filter (\( _, sid ) -> sessionUser sid model /= Nothing)
        |> List.map (\( cid, _ ) -> Lamdera.sendToFrontend cid message)
        |> Cmd.batch
