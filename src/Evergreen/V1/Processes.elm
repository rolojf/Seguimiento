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


type alias TipRef =
    { process : String
    , task : String
    }


type Tooltip
    = Hidden
    | Hovered TipRef
    | Focused TipRef


type Model
    = Model Editor String Tooltip


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
    | HoverTip TipRef
    | FocusTip TipRef
    | LeaveTip TipRef
    | BlurTip TipRef
    | CloseTip
