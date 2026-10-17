module Evergreen.V1.Pending exposing (..)

import Evergreen.V1.Domain


type RecordState
    = Active
    | Finished
    | Discarded


type DraftTarget
    = Adding
    | Saved Evergreen.V1.Domain.Ref RecordState


type DraftContent
    = TaskDraft String Evergreen.V1.Domain.PendingStatus
    | BlankDraft
    | SeparatorDraft String


type alias Draft =
    { target : DraftTarget
    , content : DraftContent
    , owner : Evergreen.V1.Domain.Owner
    , shift : String
    , orderRevision : Int
    , completion : Evergreen.V1.Domain.CompletionEdit
    }


type alias Drag =
    { source : Evergreen.V1.Domain.Ref
    , owner : Evergreen.V1.Domain.Owner
    , revision : Int
    , ids : List String
    , destination : Maybe String
    }


type alias ReviewWait =
    { ref : Evergreen.V1.Domain.Ref
    , token : Int
    , deadline : Float
    }


type Ui
    = Browsing
    | Editing Draft
    | Dragging Drag
    | Reviewing ReviewWait


type ItemFilter
    = ActiveItems
    | CompletedItems
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
    , nextReview : Int
    }


type Msg
    = AddTask
    | AddSeparator
    | Edit String
    | ContentChanged String
    | KindChanged String
    | OwnerChanged String
    | ShiftChanged String
    | CompletionChanged String
    | Save
    | Retry
    | Refresh
    | Cancel
    | DiscardEdit
    | RestoreEdit
    | PurgeEdit
    | Toggle Evergreen.V1.Domain.Ref
    | ReviewClick Evergreen.V1.Domain.Ref Float
    | ReviewElapsed ReviewWait
    | Confirm Evergreen.V1.Domain.Ref
    | DiscardRow Evergreen.V1.Domain.Ref
    | PurgeLast Evergreen.V1.Domain.Owner Evergreen.V1.Domain.Ref
    | DeleteSeparator Evergreen.V1.Domain.Ref
    | Filter ItemFilter
    | BeginDrag Drag
    | HoverRow Evergreen.V1.Domain.Owner String
    | LeaveRow Evergreen.V1.Domain.Owner String
    | LeaveGroup Evergreen.V1.Domain.Owner
    | ReleaseDrag
    | CancelDrag
    | FocusedInput
