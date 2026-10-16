module Evergreen.V1.Dashboard exposing (..)

import Evergreen.V1.Opportunities
import Evergreen.V1.Pending
import Evergreen.V1.Processes


type Section
    = MapSection
    | ProcessSection
    | PendingSection


type alias Model =
    { opportunities : Evergreen.V1.Opportunities.Model
    , processes : Evergreen.V1.Processes.Model
    , pending : Evergreen.V1.Pending.Model
    , active : Maybe Section
    , message : Maybe (Result String String)
    }


type Msg
    = OpportunityMsg Evergreen.V1.Opportunities.Msg
    | ProcessMsg Evergreen.V1.Processes.Msg
    | PendingMsg Evergreen.V1.Pending.Msg
