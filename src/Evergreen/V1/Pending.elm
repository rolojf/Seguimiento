module Evergreen.V1.Pending exposing (..)

import Evergreen.V1.Domain


type alias Model =
    { editor : Maybe ( Evergreen.V1.Domain.Target, Evergreen.V1.Domain.PendingFields )
    , submitting : Evergreen.V1.Domain.SaveState
    , error : Maybe String
    }


type Msg
    = Add
    | Edit String
    | TitleChanged String
    | OwnerChanged String
    | Save
    | Cancel
    | Retry
    | Toggle String
