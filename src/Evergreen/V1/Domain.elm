module Evergreen.V1.Domain exposing (..)

import Dict


type User
    = UserOne
    | UserTwo


type Category
    = Ventex
    | Contractor


type Kind
    = OpportunityKind
    | Responsibility


type Rating
    = One
    | Two
    | Three
    | Four
    | Five


type alias OpportunityFields =
    { title : String
    , category : Category
    , kind : Kind
    , x : Rating
    , y : Rating
    , z : Rating
    , notes : String
    , source : String
    , proposed : Bool
    }


type Review
    = Reviewed
    | NeedsReview User


type alias Opportunity =
    { id : String
    , fields : OpportunityFields
    , dx : Float
    , dy : Float
    , deleted : Bool
    , version : Int
    , createdAt : String
    , updatedAt : String
    , review : Review
    }


type Completion
    = Todo
    | Done


type PendingContent
    = PendingTask String Completion
    | Separator String


type Owner
    = Rolando
    | Dot
    | Others


type alias PendingFields =
    { content : PendingContent
    , owner : Owner
    }


type alias Pending =
    { id : String
    , fields : PendingFields
    , deleted : Bool
    , version : Int
    , createdAt : String
    , updatedAt : String
    , review : Review
    , position : Int
    }


type ProcessKind
    = NewOti
    | Deliveries


type alias ProcessFields =
    { client : String
    , kind : ProcessKind
    , startDate : String
    , context : String
    }


type Applicability
    = Applicable Completion
    | NotApplicable Completion


type alias TaskState =
    { applicability : Applicability
    , evidence : Maybe String
    , applicabilityReview : Review
    , completionReview : Review
    }


type Closure
    = Open
    | PersonallyClosed String


type alias Process =
    { id : String
    , fields : ProcessFields
    , states : Dict.Dict String TaskState
    , closure : Closure
    , legacySummary : String
    , templateVersion : Int
    , version : Int
    , createdAt : String
    , updatedAt : String
    }


type alias Board =
    { opportunities : Dict.Dict String Opportunity
    , pending : Dict.Dict String Pending
    , processes : Dict.Dict String Process
    , nextId : Int
    , revision : Int
    , pendingOrderRevision : Int
    }


type alias Ref =
    { id : String
    , version : Int
    }


type Target
    = New
    | Existing Ref


type alias Placement =
    { x : Rating
    , y : Rating
    , dx : Float
    , dy : Float
    }


type alias Archive =
    { checks : String
    , snapshots : String
    }


type Checkbox
    = ApplicabilityBox
    | CompletionBox


type TaskChange
    = SetCompletion Completion
    | ExcludeTask
    | RestoreTask
    | Acknowledge Checkbox


type alias PendingShift =
    { revision : Int
    , steps : Int
    }


type PendingMove
    = KeepPosition
    | ShiftPosition PendingShift


type Change
    = SaveOpportunity Target OpportunityFields
    | MoveOpportunity Ref Placement
    | ValidateOpportunity Ref
    | ConfirmOpportunity Ref
    | RemoveOpportunity Ref
    | RestoreOpportunity Ref
    | SavePending Target PendingFields PendingMove
    | MovePending Ref PendingShift
    | CompletePending Ref Completion
    | ConfirmPending Ref
    | RemovePending Ref
    | RestorePending Ref
    | PurgePending Ref
    | CreateProcess ProcessFields
    | ChangeTask Ref String TaskChange
