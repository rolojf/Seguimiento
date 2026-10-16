module Evergreen.V1.Processes exposing (..)

import Evergreen.V1.Domain


type alias Draft =
    { client : String
    , kind : Maybe Evergreen.V1.Domain.ProcessKind
    , startDate : String
    , context : String
    }


type Editor
    = Browsing
    | Creating Draft
    | EditingReason Evergreen.V1.Domain.Ref String String


type Model
    = Model Editor String


type Field
    = Client
    | Kind
    | StartDate
    | Context


type Msg
    = AddProcess
    | Cancel
    | Edit Field String
    | Create
    | ChangeStatus Evergreen.V1.Domain.Ref String Evergreen.V1.Domain.TaskChange
    | EditReason Evergreen.V1.Domain.Ref String
    | SetReason String
    | SaveReason
    | RetryReason
