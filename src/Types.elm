module Types exposing (ActiveSession, BackendModel, BackendMsg(..), FrontendModel, FrontendMsg(..), LoginForm, LoginState(..), PrivateData, Session, ToBackend(..), ToFrontend(..), User(..), Work(..))

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dashboard
import Dict exposing (Dict)
import Domain
import File exposing (File)
import Lamdera exposing (ClientId, SessionId)
import Time
import Url exposing (Url)


type User
    = UserOne
    | UserTwo


type alias FrontendModel =
    { key : Key
    , login : LoginState
    , nextRequest : Int
    }



-- Drafts and private data leave the model together when the session ends.


type LoginState
    = CheckingSession
    | SignedOut LoginForm
    | SigningIn String
    | SignedIn ActiveSession
    | SigningOut


type alias ActiveSession =
    { data : PrivateData
    , dashboard : Dashboard.Model
    , work : Maybe Work
    }


type Work
    = ChangeRequest Int
    | ImportRequest Int
    | ExportRequest Int


type alias LoginForm =
    { username : String
    , password : String
    , error : Maybe String
    }


type alias PrivateData =
    { user : User
    , board : Domain.Board
    }


type alias Session =
    { user : User
    , keyHash : String
    }


type alias BackendModel =
    { board : Domain.Board
    , archive : Domain.Archive
    , sessions : Dict SessionId Session
    , clients : Dict ClientId SessionId
    }


type FrontendMsg
    = UrlRequested UrlRequest
    | UrlChanged Url
    | UsernameChanged String
    | PasswordChanged String
    | SignIn
    | SignOut
    | BoardMsg Dashboard.Msg
    | ImportBackup
    | BackupSelected Int File
    | BackupLoaded Int String
    | ExportBackup


type ToBackend
    = LogIn String String
    | LogOut
    | ChangeBoard Int Domain.Change
    | ImportBoard Int String
    | ExportBoard Int


type BackendMsg
    = OnConnect SessionId ClientId
    | OnDisconnect SessionId ClientId
    | ApplyChange SessionId ClientId Session Int Domain.Change Time.Posix
    | ApplyImport SessionId ClientId Session Int String
    | PrepareExport SessionId ClientId Session Int Time.Posix


type ToFrontend
    = SessionStatus (Maybe PrivateData)
    | LoginRejected
    | BoardChanged Domain.Board
    | ChangeResult Int (Result String Domain.Board)
    | ExportReady Int String
