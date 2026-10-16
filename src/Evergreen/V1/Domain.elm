module Evergreen.V1.Domain exposing (..)

import Dict


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


type alias Opportunity =
    { id : String
    , fields : OpportunityFields
    , dx : Float
    , dy : Float
    , deleted : Bool
    , version : Int
    , createdAt : String
    , updatedAt : String
    }


type Owner
    = Rolando
    | Dot
    | Others


type Completion
    = Todo
    | Done


type alias PendingFields =
    { title : String
    , owner : Owner
    , completion : Completion
    }


type alias Pending =
    { id : String
    , fields : PendingFields
    , deleted : Bool
    , version : Int
    , createdAt : String
    , updatedAt : String
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
    | NotApplicable Completion String


type alias TaskState =
    { applicability : Applicability
    , evidence : Maybe String
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


type SaveState
    = Ready
    | Saving


type alias Archive =
    { checks : String
    , snapshots : String
    }


type TaskChange
    = SetCompletion Completion
    | ExcludeTask String
    | RestoreTask


type Change
    = SaveOpportunity Target OpportunityFields
    | MoveOpportunity Ref Placement
    | ValidateOpportunity Ref
    | RemoveOpportunity Ref
    | RestoreOpportunity Ref
    | SavePending Target PendingFields
    | CompletePending Ref Completion
    | RemovePending Ref
    | CreateProcess ProcessFields
    | ChangeTask Ref String TaskChange
