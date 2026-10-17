module Evergreen.V1.Processes exposing (..)

import Evergreen.V1.Domain


type alias Draft =
    { client : String
    , kind : Maybe Evergreen.V1.Domain.ProcessKind
    , startDate : String
    , context : String
    }


type NotesAccess
    = EditableNotes
    | ReadOnlyNotes


type alias NotesDraft =
    { ref : Evergreen.V1.Domain.Ref
    , task : String
    , title : String
    , notes : String
    , opener : String
    }


type Editor
    = Browsing
    | Creating Draft
    | TakingNotes NotesAccess NotesDraft
    | Completing Evergreen.V1.Domain.Ref String
    | Purging Evergreen.V1.Domain.Ref String


type alias TipRef =
    { process : String
    , task : String
    }


type Tooltip
    = Hidden
    | Hovered TipRef
    | Focused TipRef


type Model
    = Model
        { editor : Editor
        , error : String
        , tooltip : Tooltip
        , filter : Evergreen.V1.Domain.ProcessStatus
        , focus : Maybe String
        }


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
    | SelectFilter Evergreen.V1.Domain.ProcessStatus
    | ChangeStatus Evergreen.V1.Domain.Ref String Evergreen.V1.Domain.TaskChange
    | OpenNotes NotesAccess NotesDraft
    | EditNotes String
    | SaveNotes
    | AskComplete Evergreen.V1.Domain.Ref String
    | AskPurge Evergreen.V1.Domain.Ref String
    | ConfirmAction
    | CloseDialog
    | FocusAt String
    | FocusNext
    | FocusedElement
    | HoverTip TipRef
    | FocusTip TipRef
    | LeaveTip TipRef
    | BlurTip TipRef
    | CloseTip
