module Evergreen.V1.Pending exposing (..)

import Evergreen.V1.Domain


type RecordState
    = Active
    | Discarded


type DraftTarget
    = Adding
    | Saved Evergreen.V1.Domain.Ref RecordState


type DraftContent
    = TaskDraft String Evergreen.V1.Domain.Completion
    | BlankDraft
    | SeparatorDraft String


type alias Draft =
    { target : DraftTarget
    , content : DraftContent
    , owner : Evergreen.V1.Domain.Owner
    , shift : String
    , orderRevision : Int
    }


type alias Drag =
    { source : Evergreen.V1.Domain.Ref
    , owner : Evergreen.V1.Domain.Owner
    , revision : Int
    , ids : List String
    , destination : Maybe String
    }


type Ui
    = Browsing
    | Editing Draft
    | Dragging Drag


type ItemFilter
    = ActiveItems
    | DiscardedItems


type Submission
    = Idle
    | SaveChange
    | StatusChange


type alias Model =
    { ui : Ui
    , filter : ItemFilter
    , submitting : Submission
    , error : Maybe String
    }


type Msg
    = AddTask
    | AddSeparator
    | Edit String
    | ContentChanged String
    | KindChanged String
    | OwnerChanged String
    | ShiftChanged String
    | Save
    | Retry
    | Refresh
    | Cancel
    | DiscardEdit
    | RestoreEdit
    | PurgeEdit
    | Toggle Evergreen.V1.Domain.Ref Evergreen.V1.Domain.Completion
    | Confirm Evergreen.V1.Domain.Ref
    | DiscardRow Evergreen.V1.Domain.Ref
    | Filter ItemFilter
    | BeginDrag Drag
    | HoverRow Evergreen.V1.Domain.Owner String
    | LeaveRow Evergreen.V1.Domain.Owner String
    | LeaveGroup Evergreen.V1.Domain.Owner
    | ReleaseDrag
    | CancelDrag
