module Evergreen.V1.Opportunities exposing (..)

import Browser.Dom
import Evergreen.V1.Domain


type Panel
    = Closed
    | Detail String
    | Editor Evergreen.V1.Domain.Target Evergreen.V1.Domain.OpportunityFields
    | ConfirmRemoval Evergreen.V1.Domain.Ref


type Intent
    = SaveForm Evergreen.V1.Domain.Target
    | DeleteItem String
    | RestoreItem String
    | ChangeItem


type Request
    = Idle
    | Waiting Intent


type alias Point =
    { x : Float
    , y : Float
    }


type alias Drag =
    { token : Int
    , ref : Evergreen.V1.Domain.Ref
    , pointerId : Int
    , start : Point
    , current : Point
    , original : Evergreen.V1.Domain.Placement
    }


type DragState
    = Still
    | Measuring Drag
    | Moving Drag Browser.Dom.Element


type alias Model =
    { category : Maybe Evergreen.V1.Domain.Category
    , kind : Maybe Evergreen.V1.Domain.Kind
    , panel : Panel
    , request : Request
    , undo : Maybe String
    , error : Maybe String
    , drag : DragState
    , dragToken : Int
    , skipClick : Maybe String
    }


type TextField
    = Title
    | Notes
    | Source


type Axis
    = Ease
    | Nearness
    | Potential


type Validation
    = Proposed
    | Validated


type Direction
    = Left
    | Right
    | Up
    | Down


type Step
    = Cell
    | Fine


type Msg
    = FilterCategory (Maybe Evergreen.V1.Domain.Category)
    | FilterKind (Maybe Evergreen.V1.Domain.Kind)
    | Select String
    | Add
    | Edit String
    | Close
    | Escape
    | SetText TextField String
    | SetCategory Evergreen.V1.Domain.Category
    | SetKind Evergreen.V1.Domain.Kind
    | SetRating Axis Evergreen.V1.Domain.Rating
    | SetValidation Validation
    | Save
    | Validate String
    | AskRemove String
    | Remove
    | Undo
    | DismissUndo
    | StartDrag String Int Point
    | GotBounds Int (Result Browser.Dom.Error Browser.Dom.Element)
    | MovePointer Int Point
    | EndDrag Int Point
    | CancelDrag
    | MoveKey String Direction Step
