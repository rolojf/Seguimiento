module Dashboard exposing (Model, Msg, init, isLocal, notice, settle, subscriptions, update, view)

import Domain exposing (Board, Change, SaveState, User)
import Html exposing (Html)
import Html.Attributes as A
import Opportunities
import Pending
import Processes


type Section
    = MapSection
    | ProcessSection
    | PendingSection


type alias Model =
    { opportunities : Opportunities.Model
    , processes : Processes.Model
    , pending : Pending.Model
    , active : Maybe Section
    , message : Maybe (Result String String)
    }


type Msg
    = OpportunityMsg Opportunities.Msg
    | ProcessMsg Processes.Msg
    | PendingMsg Pending.Msg


init : Model
init =
    { opportunities = Opportunities.init
    , processes = Processes.init
    , pending = Pending.init
    , active = Nothing
    , message = Nothing
    }


isLocal : Msg -> Bool
isLocal msg =
    case msg of
        ProcessMsg action ->
            Processes.isLocal action

        PendingMsg action ->
            Pending.isLocal action

        _ ->
            False


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ProcessMsg (Processes.subscriptions model.processes)
        , Sub.map PendingMsg (Pending.subscriptions model.pending)
        ]


update : Msg -> Board -> Model -> ( Model, Cmd Msg, Maybe Change )
update msg board model =
    case msg of
        OpportunityMsg action ->
            let
                ( section, cmd, change ) =
                    Opportunities.update action board model.opportunities
            in
            ( markActive MapSection change { model | opportunities = section }
            , Cmd.map OpportunityMsg cmd
            , change
            )

        ProcessMsg action ->
            let
                ( section, change ) =
                    Processes.update action board model.processes
            in
            ( markActive ProcessSection change { model | processes = section }, Cmd.none, change )

        PendingMsg action ->
            let
                ( section, change ) =
                    Pending.update action board model.pending
            in
            ( markActive PendingSection change { model | pending = section }, Cmd.none, change )


markActive : Section -> Maybe Change -> Model -> Model
markActive section change model =
    case change of
        Just _ ->
            { model | active = Just section, message = Just (Ok "Guardando…") }

        Nothing ->
            model


settle : Result String () -> Model -> Model
settle result model =
    let
        updated =
            case model.active of
                Just MapSection ->
                    { model | opportunities = Opportunities.settle result model.opportunities }

                Just ProcessSection ->
                    { model | processes = Processes.settle result model.processes }

                Just PendingSection ->
                    { model | pending = Pending.settle result model.pending }

                Nothing ->
                    model
    in
    { updated | active = Nothing, message = Just (Result.map (\_ -> "Cambio guardado") result) }


notice : Result String String -> Model -> Model
notice message model =
    { model | message = Just message }


view : SaveState -> User -> Board -> Model -> Html Msg
view state user board model =
    Html.main_ [ A.class "mx-auto max-w-7xl space-y-7 px-4 py-7 sm:px-6" ]
        [ case model.message of
            Just (Err error) ->
                Html.p [ A.attribute "role" "alert", A.class "rounded-lg border border-red-200 bg-red-50 px-4 py-3 text-base text-red-800" ] [ Html.text error ]

            Just (Ok message) ->
                Html.p [ A.attribute "role" "status", A.class "text-base text-slate-600" ] [ Html.text message ]

            Nothing ->
                Html.text ""
        , Html.map OpportunityMsg (Opportunities.view state user board model.opportunities)
        , Html.map ProcessMsg (Processes.view state user board model.processes)
        , Html.map PendingMsg (Pending.view state user board model.pending)
        , Html.footer [ A.class "pb-6 text-center text-base text-slate-500" ]
            [ Html.text "Los cambios se guardan en Lamdera y se comparten entre los dos usuarios." ]
        ]
