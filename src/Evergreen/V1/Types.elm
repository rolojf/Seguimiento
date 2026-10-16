module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Evergreen.V1.Dashboard
import Evergreen.V1.Domain
import File
import Lamdera
import Time
import Url


type alias LoginForm =
    { username : String
    , password : String
    , error : Maybe String
    }


type User
    = UserOne
    | UserTwo


type alias PrivateData =
    { user : User
    , board : Evergreen.V1.Domain.Board
    }


type Work
    = ChangeRequest Int
    | ImportRequest Int
    | ExportRequest Int


type alias ActiveSession =
    { data : PrivateData
    , dashboard : Evergreen.V1.Dashboard.Model
    , work : Maybe Work
    }


type LoginState
    = CheckingSession
    | SignedOut LoginForm
    | SigningIn String
    | SignedIn ActiveSession
    | SigningOut


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , login : LoginState
    , nextRequest : Int
    }


type alias Session =
    { user : User
    , keyHash : String
    }


type alias BackendModel =
    { board : Evergreen.V1.Domain.Board
    , archive : Evergreen.V1.Domain.Archive
    , sessions : Dict.Dict Lamdera.SessionId Session
    , clients : Dict.Dict Lamdera.ClientId Lamdera.SessionId
    }


type FrontendMsg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | UsernameChanged String
    | PasswordChanged String
    | SignIn
    | SignOut
    | BoardMsg Evergreen.V1.Dashboard.Msg
    | ImportBackup
    | BackupSelected Int File.File
    | BackupLoaded Int String
    | ExportBackup


type ToBackend
    = LogIn String String
    | LogOut
    | ChangeBoard Int Evergreen.V1.Domain.Change
    | ImportBoard Int String
    | ExportBoard Int


type BackendMsg
    = OnConnect Lamdera.SessionId Lamdera.ClientId
    | OnDisconnect Lamdera.SessionId Lamdera.ClientId
    | ApplyChange Lamdera.SessionId Lamdera.ClientId Session Int Evergreen.V1.Domain.Change Time.Posix
    | ApplyImport Lamdera.SessionId Lamdera.ClientId Session Int String
    | PrepareExport Lamdera.SessionId Lamdera.ClientId Session Int Time.Posix


type ToFrontend
    = SessionStatus (Maybe PrivateData)
    | LoginRejected
    | BoardChanged Evergreen.V1.Domain.Board
    | ChangeResult Int (Result String Evergreen.V1.Domain.Board)
    | ExportReady Int String
