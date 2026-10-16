module Opportunities exposing (Model, Msg, init, settle, update, view)

import Browser.Dom as Dom
import Dict
import Domain exposing (Board, Category(..), Change(..), Kind(..), Opportunity, OpportunityFields, Placement, Rating(..), Ref, SaveState(..), Target(..))
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode
import Task
import Url


type alias Model =
    { category : Maybe Category
    , kind : Maybe Kind
    , panel : Panel
    , request : Request
    , undo : Maybe String
    , error : Maybe String
    , drag : DragState
    , dragToken : Int
    , skipClick : Maybe String
    }


type Panel
    = Closed
    | Detail String
    | Editor Target OpportunityFields
    | ConfirmRemoval Ref


type Request
    = Idle
    | Waiting Intent


type Intent
    = SaveForm Target
    | DeleteItem String
    | RestoreItem String
    | ChangeItem


type DragState
    = Still
    | Measuring Drag
    | Moving Drag Dom.Element


type alias Drag =
    { token : Int
    , ref : Ref
    , pointerId : Int
    , start : Point
    , current : Point
    , original : Placement
    }


type alias Point =
    { x : Float, y : Float }


type TextField
    = Title
    | Notes
    | Source


type Axis
    = Ease
    | Nearness
    | Potential


type Direction
    = Left
    | Right
    | Up
    | Down


type Step
    = Cell
    | Fine


type Validation
    = Proposed
    | Validated


type Msg
    = FilterCategory (Maybe Category)
    | FilterKind (Maybe Kind)
    | Select String
    | Add
    | Edit String
    | Close
    | Escape
    | SetText TextField String
    | SetCategory Category
    | SetKind Kind
    | SetRating Axis Rating
    | SetValidation Validation
    | Save
    | Validate String
    | AskRemove String
    | Remove
    | Undo
    | DismissUndo
    | StartDrag String Int Point
    | GotBounds Int (Result Dom.Error Dom.Element)
    | MovePointer Int Point
    | EndDrag Int Point
    | CancelDrag
    | MoveKey String Direction Step


init : Model
init =
    { category = Nothing
    , kind = Nothing
    , panel = Closed
    , request = Idle
    , undo = Nothing
    , error = Nothing
    , drag = Still
    , dragToken = 0
    , skipClick = Nothing
    }


plotId : String
plotId =
    "opportunity-map"


offsetLimit : Float
offsetLimit =
    0.32


fineStep : Float
fineStep =
    0.06


dragThreshold : Float
dragThreshold =
    5


blank : OpportunityFields
blank =
    { title = ""
    , category = Ventex
    , kind = OpportunityKind
    , x = Three
    , y = Three
    , z = Three
    , notes = ""
    , source = ""
    , proposed = True
    }


update : Msg -> Board -> Model -> ( Model, Cmd Msg, Maybe Change )
update msg board model =
    case model.request of
        Waiting _ ->
            unchanged model

        Idle ->
            updateReady msg board model


updateReady : Msg -> Board -> Model -> ( Model, Cmd Msg, Maybe Change )
updateReady msg board model =
    case msg of
        FilterCategory category ->
            unchanged { model | category = category, drag = Still }

        FilterKind kind ->
            unchanged { model | kind = kind, drag = Still }

        Select id ->
            if model.skipClick == Just id then
                unchanged { model | skipClick = Nothing }

            else
                unchanged { model | panel = Detail id, error = Nothing, skipClick = Nothing }

        Add ->
            unchanged
                { model
                    | panel =
                        Editor New
                            { blank
                                | category = Maybe.withDefault Ventex model.category
                                , kind = Maybe.withDefault OpportunityKind model.kind
                            }
                    , error = Nothing
                    , drag = Still
                }

        Edit id ->
            case active id board of
                Just item ->
                    unchanged { model | panel = Editor (Existing (Domain.ref item)) item.fields, error = Nothing, drag = Still }

                Nothing ->
                    unchanged { model | panel = Closed }

        Close ->
            unchanged { model | panel = Closed, error = Nothing, drag = Still }

        Escape ->
            case model.drag of
                Still ->
                    case model.panel of
                        ConfirmRemoval ref ->
                            unchanged { model | panel = Detail ref.id }

                        _ ->
                            unchanged { model | panel = Closed, error = Nothing }

                _ ->
                    unchanged (cancelDrag model)

        SetText textField value ->
            editFields (setText textField value) model

        SetCategory category ->
            editFields (\fields -> { fields | category = category }) model

        SetKind kind ->
            editFields (\fields -> { fields | kind = kind }) model

        SetRating axis rating ->
            editFields (setRating axis rating) model

        SetValidation validation ->
            editFields (\fields -> { fields | proposed = validation == Proposed }) model

        Save ->
            case model.panel of
                Editor target fields ->
                    let
                        clean =
                            { fields | title = String.trim fields.title, notes = String.trim fields.notes, source = String.trim fields.source }
                    in
                    if String.isEmpty clean.title then
                        unchanged { model | error = Just "Escribe el nombre del elemento." }

                    else if clean.source /= "" && Url.fromString clean.source == Nothing then
                        unchanged { model | error = Just "La fuente debe ser un enlace http:// o https://." }

                    else
                        submit (SaveForm target) (SaveOpportunity target clean)
                            { model
                                | category = keepFilter clean.category model.category
                                , kind = keepFilter clean.kind model.kind
                            }

                _ ->
                    unchanged model

        Validate id ->
            case active id board of
                Just item ->
                    submit ChangeItem (ValidateOpportunity (Domain.ref item)) model

                Nothing ->
                    unchanged model

        AskRemove id ->
            case active id board of
                Just item ->
                    unchanged { model | panel = ConfirmRemoval (Domain.ref item), error = Nothing }

                Nothing ->
                    unchanged model

        Remove ->
            case model.panel of
                ConfirmRemoval ref ->
                    submit (DeleteItem ref.id) (RemoveOpportunity ref) model

                _ ->
                    unchanged model

        Undo ->
            case model.undo |> Maybe.andThen (\id -> Dict.get id board.opportunities) of
                Just item ->
                    if item.deleted then
                        submit (RestoreItem item.id) (RestoreOpportunity (Domain.ref item)) model

                    else
                        unchanged { model | undo = Nothing }

                Nothing ->
                    unchanged model

        DismissUndo ->
            unchanged { model | undo = Nothing }

        StartDrag id pointerId point ->
            startDrag id pointerId point board model

        GotBounds token result ->
            case ( model.drag, result ) of
                ( Measuring drag, Ok bounds ) ->
                    if drag.token == token && bounds.element.width > 0 && bounds.element.height > 0 then
                        unchanged { model | drag = Moving drag bounds }

                    else
                        unchanged model

                ( Measuring drag, Err _ ) ->
                    if drag.token == token then
                        unchanged { model | drag = Still }

                    else
                        unchanged model

                _ ->
                    unchanged model

        MovePointer pointerId point ->
            unchanged (movePointer pointerId point model)

        EndDrag pointerId point ->
            endDrag pointerId point model

        CancelDrag ->
            unchanged (cancelDrag model)

        MoveKey id direction step ->
            case model.panel of
                Editor _ _ ->
                    unchanged model

                _ ->
                    case active id board of
                        Just item ->
                            let
                                next =
                                    keyboardPlacement direction step (placement item)
                            in
                            if next == placement item then
                                unchanged model

                            else
                                submit ChangeItem (MoveOpportunity (Domain.ref item) next) model

                        Nothing ->
                            unchanged model


unchanged : Model -> ( Model, Cmd Msg, Maybe Change )
unchanged model =
    ( model, Cmd.none, Nothing )


submit : Intent -> Change -> Model -> ( Model, Cmd Msg, Maybe Change )
submit intent change model =
    ( { model | request = Waiting intent, error = Nothing, drag = Still }, Cmd.none, Just change )


settle : Result String () -> Model -> Model
settle result model =
    case model.request of
        Idle ->
            model

        Waiting intent ->
            case result of
                Err message ->
                    -- A rejected write keeps the editor and its original version.
                    { model | request = Idle, error = Just message, skipClick = Nothing }

                Ok () ->
                    let
                        ready =
                            { model | request = Idle, error = Nothing, skipClick = Nothing }
                    in
                    case intent of
                        SaveForm New ->
                            { ready | panel = Closed }

                        SaveForm (Existing ref) ->
                            { ready | panel = Detail ref.id }

                        DeleteItem id ->
                            { ready | panel = Closed, undo = Just id }

                        RestoreItem id ->
                            { ready | panel = Detail id, undo = Nothing, category = Nothing, kind = Nothing }

                        ChangeItem ->
                            ready


keepFilter : a -> Maybe a -> Maybe a
keepFilter value current =
    if current == Just value then
        current

    else
        Nothing


active : String -> Board -> Maybe Opportunity
active id board =
    Dict.get id board.opportunities
        |> Maybe.andThen
            (\item ->
                if item.deleted then
                    Nothing

                else
                    Just item
            )


editFields : (OpportunityFields -> OpportunityFields) -> Model -> ( Model, Cmd Msg, Maybe Change )
editFields change model =
    case model.panel of
        Editor target fields ->
            unchanged { model | panel = Editor target (change fields), error = Nothing }

        _ ->
            unchanged model


setText : TextField -> String -> OpportunityFields -> OpportunityFields
setText textField value fields =
    case textField of
        Title ->
            { fields | title = value }

        Notes ->
            { fields | notes = value }

        Source ->
            { fields | source = value }


setRating : Axis -> Rating -> OpportunityFields -> OpportunityFields
setRating axis rating fields =
    case axis of
        Ease ->
            { fields | x = rating }

        Nearness ->
            { fields | y = rating }

        Potential ->
            { fields | z = rating }


placement : Opportunity -> Placement
placement item =
    { x = item.fields.x, y = item.fields.y, dx = item.dx, dy = item.dy }


startDrag : String -> Int -> Point -> Board -> Model -> ( Model, Cmd Msg, Maybe Change )
startDrag id pointerId point board model =
    case model.panel of
        Editor _ _ ->
            unchanged model

        _ ->
            case active id board of
                Just item ->
                    let
                        token =
                            model.dragToken + 1

                        drag =
                            { token = token, ref = Domain.ref item, pointerId = pointerId, start = point, current = point, original = placement item }
                    in
                    ( { model | drag = Measuring drag, dragToken = token, skipClick = Nothing }
                    , Dom.getElement plotId |> Task.attempt (GotBounds token)
                    , Nothing
                    )

                Nothing ->
                    unchanged model


movePointer : Int -> Point -> Model -> Model
movePointer pointerId point model =
    case model.drag of
        Measuring drag ->
            if drag.pointerId == pointerId then
                { model | drag = Measuring { drag | current = point } }

            else
                model

        Moving drag bounds ->
            if drag.pointerId == pointerId then
                { model | drag = Moving { drag | current = point } bounds }

            else
                model

        Still ->
            model


endDrag : Int -> Point -> Model -> ( Model, Cmd Msg, Maybe Change )
endDrag pointerId point model =
    case (movePointer pointerId point model).drag of
        Moving drag bounds ->
            if drag.pointerId == pointerId then
                if moved drag then
                    submit ChangeItem (MoveOpportunity drag.ref (dragPlacement drag bounds))
                        { model | skipClick = Just drag.ref.id }

                else
                    unchanged { model | drag = Still }

            else
                unchanged model

        Measuring drag ->
            if drag.pointerId == pointerId then
                unchanged (cancelDrag model)

            else
                unchanged model

        Still ->
            unchanged model


cancelDrag : Model -> Model
cancelDrag model =
    let
        skip drag =
            if moved drag then
                Just drag.ref.id

            else
                model.skipClick
    in
    case model.drag of
        Measuring drag ->
            { model | drag = Still, skipClick = skip drag }

        Moving drag _ ->
            { model | drag = Still, skipClick = skip drag }

        Still ->
            model


moved : Drag -> Bool
moved drag =
    (drag.current.x - drag.start.x) ^ 2 + (drag.current.y - drag.start.y) ^ 2 >= dragThreshold ^ 2


dragPlacement : Drag -> Dom.Element -> Placement
dragPlacement drag bounds =
    let
        -- Both pointer coordinates and Browser.Dom coordinates use the document.
        column =
            toFloat (Domain.rating drag.original.x) - 0.5 + drag.original.dx + (drag.current.x - drag.start.x) / (bounds.element.width / 5)

        row =
            5.5 - toFloat (Domain.rating drag.original.y) + drag.original.dy + (drag.current.y - drag.start.y) / (bounds.element.height / 5)

        x =
            Domain.fromRating (floor column + 1)

        y =
            Domain.fromRating (5 - floor row)
    in
    { x = x
    , y = y
    , dx = clampOffset (column - (toFloat (Domain.rating x) - 0.5))
    , dy = clampOffset (row - (5.5 - toFloat (Domain.rating y)))
    }


clampOffset : Float -> Float
clampOffset value =
    toFloat (round (clamp -offsetLimit offsetLimit value * 10000)) / 10000


keyboardPlacement : Direction -> Step -> Placement -> Placement
keyboardPlacement direction step current =
    let
        ( horizontal, vertical ) =
            case direction of
                Left ->
                    ( -1, 0 )

                Right ->
                    ( 1, 0 )

                Up ->
                    ( 0, 1 )

                Down ->
                    ( 0, -1 )
    in
    case step of
        Cell ->
            { current
                | x = Domain.fromRating (Domain.rating current.x + horizontal)
                , y = Domain.fromRating (Domain.rating current.y + vertical)
            }

        Fine ->
            { current
                | dx = clampOffset (current.dx + toFloat horizontal * fineStep)
                , dy = clampOffset (current.dy - toFloat vertical * fineStep)
            }


view : SaveState -> Board -> Model -> Html Msg
view saveState board model =
    let
        state =
            if saveState == Saving || model.request /= Idle then
                Saving

            else
                Ready

        items =
            Dict.values board.opportunities
                |> List.filter (\item -> not item.deleted)
                |> List.sortBy (\item -> ( item.createdAt, item.id ))

        visible =
            List.filter
                (\item ->
                    (model.category == Nothing || model.category == Just item.fields.category)
                        && (model.kind == Nothing || model.kind == Just item.fields.kind)
                )
                items
    in
    Html.section
        [ Attr.id "oportunidades"
        , Attr.class "rounded-2xl border border-stone-200 bg-white p-4 shadow-sm sm:p-6"
        , Attr.attribute "aria-labelledby" "opportunities-heading"
        , Events.custom "keydown" (keyEvent escapeDecoder)
        ]
        [ Html.div [ Attr.class "flex flex-wrap items-start justify-between gap-4" ]
            [ Html.div []
                [ Html.p [ Attr.class "text-xs font-semibold uppercase tracking-widest text-emerald-700" ] [ Html.text "Dónde enfocar tu esfuerzo" ]
                , Html.h2 [ Attr.id "opportunities-heading", Attr.class "mt-1 text-xl font-semibold text-stone-900" ] [ Html.text "Oportunidades y responsabilidades" ]
                , Html.p [ Attr.class "mt-1 text-sm text-stone-500" ] [ Html.text "Más fácil y más directa, hacia arriba a la derecha." ]
                ]
            , button state Add "Añadir" primary
            ]
        , Html.div [ Attr.class "my-4 flex flex-wrap gap-3" ]
            [ filterButtons "Categoría" model.category FilterCategory [ ( Nothing, "Ambas" ), ( Just Ventex, "Ventex" ), ( Just Contractor, "Contratista" ) ]
            , filterButtons "Tipo" model.kind FilterKind [ ( Nothing, "Ambas" ), ( Just OpportunityKind, "Oportunidades" ), ( Just Responsibility, "Responsabilidades" ) ]
            ]
        , Html.p [ Attr.class "mb-4 text-xs text-stone-600" ]
            [ Html.text (String.fromInt (List.length visible) ++ " elementos · Verde: Ventex · Ámbar: Contratista · R: responsabilidad · Borde punteado: por validar") ]
        , viewError model.error
        , viewUndo state board model.undo
        , Html.div
            [ Attr.class
                (if model.panel == Closed then
                    "grid min-w-0 gap-6"

                 else
                    "grid min-w-0 gap-6 xl:grid-cols-[minmax(0,1fr)_20rem]"
                )
            ]
            [ viewMap state visible model
            , viewPanel state board model.panel
            ]
        ]


viewMap : SaveState -> List Opportunity -> Model -> Html Msg
viewMap state items model =
    Html.div [ Attr.class "min-w-0" ]
        [ Html.p [ Attr.class "mb-2 text-xs text-stone-600" ] [ Html.text "Beneficio cercano ↑ · 5: directa / corto plazo" ]
        , Html.div [ Attr.class "overflow-x-auto pb-4" ]
            [ Html.div [ Attr.class "grid min-w-[440px] grid-cols-[1.5rem_minmax(0,1fr)]" ]
                [ Html.div [ Attr.class "grid h-[360px] grid-rows-5 items-center text-xs text-stone-500" ]
                    (List.map (\number -> Html.span [] [ Html.text (String.fromInt number) ]) [ 5, 4, 3, 2, 1 ])
                , Html.div
                    ([ Attr.id plotId
                     , Attr.class "relative h-[360px] rounded-lg bg-stone-50"
                     , Attr.attribute "role" "group"
                     , Attr.attribute "aria-label" "Mapa. Facilidad horizontal del 1 al 5; beneficio cercano vertical del 1 al 5."
                     ]
                        ++ mapEvents state
                    )
                    (Html.div [ Attr.class "pointer-events-none absolute inset-0 grid grid-cols-5 grid-rows-5 overflow-hidden rounded-lg border-l border-t border-stone-200", Attr.attribute "aria-hidden" "true" ]
                        (List.range 0 24
                            |> List.map
                                (\index ->
                                    Html.div
                                        [ Attr.class
                                            (if index < 5 && modBy 5 index >= 3 then
                                                "border-b border-r border-stone-200 bg-emerald-50"

                                             else
                                                "border-b border-r border-stone-200"
                                            )
                                        ]
                                        []
                                )
                        )
                        :: (if List.isEmpty items then
                                [ Html.p [ Attr.class "pointer-events-none absolute inset-x-6 top-36 text-center text-sm text-stone-500" ] [ Html.text "No hay elementos en este filtro. Añade uno o cambia los filtros." ] ]

                            else
                                List.map (viewBubble state model) items
                           )
                    )
                , Html.div [] []
                , Html.div [ Attr.class "mt-2 grid grid-cols-5 text-center text-xs text-stone-500", Attr.attribute "aria-hidden" "true" ]
                    (List.range 1 5 |> List.map (\number -> Html.span [] [ Html.text (String.fromInt number) ]))
                ]
            ]
        , Html.div [ Attr.class "flex justify-between gap-3 text-xs text-stone-600" ]
            [ Html.span [] [ Html.text "1: difícil / indirecta" ]
            , Html.strong [] [ Html.text "Facilidad →" ]
            , Html.span [] [ Html.text "5: fácil / simple" ]
            ]
        , Html.p [ Attr.id "opportunity-map-help", Attr.class "mt-4 border-t border-stone-100 pt-3 text-xs leading-relaxed text-stone-500" ]
            [ Html.text "Tamaño = potencial de comisión. Arrastra dentro del mapa para cambiar facilidad y cercanía; salir del mapa cancela. Flechas: una celda. Mayús + flechas: ajuste fino. Cambia el potencial en Editar." ]
        ]


viewBubble : SaveState -> Model -> Opportunity -> Html Msg
viewBubble state model item =
    let
        point =
            case model.drag of
                Moving drag bounds ->
                    if drag.ref.id == item.id && moved drag then
                        dragPlacement drag bounds

                    else
                        placement item

                _ ->
                    placement item

        size =
            String.fromInt (24 + Domain.rating item.fields.z * 6) ++ "px"

        description =
            item.fields.title ++ ". " ++ Domain.kindLabel item.fields.kind ++ ". " ++ Domain.categoryLabel item.fields.category ++ ". Facilidad " ++ String.fromInt (Domain.rating item.fields.x) ++ ", cercanía " ++ String.fromInt (Domain.rating item.fields.y) ++ ", potencial " ++ String.fromInt (Domain.rating item.fields.z) ++ " de 5."

        color =
            case item.fields.category of
                Ventex ->
                    "border-emerald-700 bg-emerald-200 text-emerald-900"

                Contractor ->
                    "border-amber-700 bg-amber-200 text-amber-900"

        selected =
            case model.panel of
                Detail id ->
                    id == item.id

                Editor (Existing ref) _ ->
                    ref.id == item.id

                ConfirmRemoval ref ->
                    ref.id == item.id

                _ ->
                    False
    in
    Html.button
        ([ Attr.type_ "button"
         , Attr.disabled (state == Saving)
         , Attr.class ("absolute z-10 -translate-x-1/2 -translate-y-1/2 cursor-grab rounded-full border-2 text-xs hover:z-20 focus:z-20 focus:outline-none focus:ring-4 focus:ring-emerald-200 disabled:cursor-wait " ++ color ++ if selected then " ring-2 ring-stone-800 ring-offset-2" else "")
         , Attr.attribute "aria-pressed" (if selected then "true" else "false")
         , Attr.style "left" (percent ((toFloat (Domain.rating point.x) - 0.5 + point.dx) * 20))
         , Attr.style "top" (percent ((5.5 - toFloat (Domain.rating point.y) + point.dy) * 20))
         , Attr.style "width" size
         , Attr.style "height" size
         , Attr.style "touch-action" "none"
         , Attr.style "user-select" "none"
         , Attr.style "border-style"
            (if item.fields.proposed then
                "dashed"

             else
                "solid"
            )
         , Attr.title description
         , Attr.attribute "aria-label" (description ++ if item.fields.proposed then " Por validar." else "")
         , Attr.attribute "aria-describedby" "opportunity-map-help"
         , Events.onClick (Select item.id)
         ]
            ++ (if state == Ready then
                    [ Events.on "pointerdown" (startDecoder item.id)
                    , Events.custom "keydown" (keyEvent (moveDecoder item.id))
                    ]

                else
                    []
               )
        )
        [ Html.text
            (if item.fields.kind == Responsibility then
                "R"

             else
                ""
            )
        , Html.span [ Attr.class "pointer-events-none absolute left-1/2 top-full mt-1 w-max max-w-[110px] -translate-x-1/2 rounded bg-white/90 px-1 text-center text-[10px] leading-tight" ]
            [ Html.text (item.fields.title |> String.words |> List.take 2 |> String.join " ") ]
        ]


percent : Float -> String
percent value =
    String.fromFloat value ++ "%"


viewPanel : SaveState -> Board -> Panel -> Html Msg
viewPanel state board panel =
    case panel of
        Closed ->
            Html.text ""

        Editor target fields ->
            panelFrame state
                (case target of
                    New ->
                        "Nuevo elemento"

                    Existing _ ->
                        "Editar elemento"
                )
                [ viewEditor state fields ]

        Detail id ->
            panelFrame state "Detalle" [ viewDetail state board id ]

        ConfirmRemoval ref ->
            panelFrame state "Eliminar elemento"
                [ Html.p [ Attr.class "text-sm text-stone-700" ] [ Html.text "¿Eliminar este elemento del mapa? Podrás deshacerlo." ]
                , Html.div [ Attr.class "mt-4 flex gap-2" ]
                    [ button state Remove "Sí, eliminar" "rounded-lg bg-red-700 px-3 py-2 text-sm text-white"
                    , button state (Select ref.id) "Cancelar" secondary
                    ]
                ]


panelFrame : SaveState -> String -> List (Html Msg) -> Html Msg
panelFrame state title content =
    Html.aside [ Attr.class "min-w-0 rounded-xl border border-stone-200 bg-stone-50 p-4", Attr.attribute "aria-label" title ]
        (Html.div [ Attr.class "mb-4 flex items-center justify-between gap-3" ]
            [ Html.h3 [ Attr.class "font-semibold text-stone-900" ] [ Html.text title ]
            , button state Close "Cerrar" "text-xs text-stone-600 underline"
            ]
            :: content
        )


viewEditor : SaveState -> OpportunityFields -> Html Msg
viewEditor state fields =
    Html.form [ Events.onSubmit Save, Attr.class "space-y-4" ]
        [ field "Nombre corto"
            (Html.input [ Attr.required True, Attr.maxlength 80, Attr.value fields.title, Attr.disabled (state == Saving), Attr.class inputClass, Events.onInput (SetText Title) ] [])
        , field "Tipo"
            (choice state (kindValue fields.kind)
                (\value -> SetKind (if value == "responsibility" then Responsibility else OpportunityKind))
                [ ( "opportunity", "Oportunidad" ), ( "responsibility", "Responsabilidad" ) ]
            )
        , field "Categoría"
            (choice state (Domain.categoryLabel fields.category)
                (\value -> SetCategory (if value == "Contratista" then Contractor else Ventex))
                [ ( "Ventex", "Ventex" ), ( "Contratista", "Contratista" ) ]
            )
        , Html.div [ Attr.class "grid grid-cols-3 gap-2" ]
            [ ratingInput state Ease "Facilidad" fields.x
            , ratingInput state Nearness "Cercanía" fields.y
            , ratingInput state Potential "Potencial" fields.z
            ]
        , Html.label [ Attr.class "flex items-start gap-2 text-sm text-stone-700" ]
            [ Html.input
                [ Attr.type_ "checkbox", Attr.checked fields.proposed, Attr.disabled (state == Saving)
                , Events.onCheck (\checked -> SetValidation (if checked then Proposed else Validated))
                ]
                []
            , Html.span [] [ Html.text "Por validar", Html.small [ Attr.class "block text-xs text-stone-500" ] [ Html.text "Valor sugerido; no es una comisión confirmada." ] ]
            ]
        , field "Notas (opcional)"
            (Html.textarea [ Attr.rows 4, Attr.maxlength 2000, Attr.value fields.notes, Attr.disabled (state == Saving), Attr.class inputClass, Events.onInput (SetText Notes) ] [])
        , field "Fuente o enlace (opcional)"
            (Html.input [ Attr.type_ "url", Attr.maxlength 2000, Attr.value fields.source, Attr.disabled (state == Saving), Attr.placeholder "https://…", Attr.class inputClass, Events.onInput (SetText Source) ] [])
        , Html.div [ Attr.class "flex gap-2" ]
            [ Html.button [ Attr.type_ "submit", Attr.disabled (state == Saving || String.isEmpty (String.trim fields.title)), Attr.class primary ] [ Html.text (if state == Saving then "Guardando…" else "Guardar") ]
            , button state Close "Cancelar" secondary
            ]
        ]


viewDetail : SaveState -> Board -> String -> Html Msg
viewDetail state board id =
    case active id board of
        Nothing ->
            Html.p [ Attr.class "text-sm text-stone-500" ] [ Html.text "Este elemento ya no está disponible." ]

        Just item ->
            Html.div [ Attr.class "space-y-4" ]
                [ Html.p [ Attr.class "text-xs text-stone-500" ] [ Html.text (Domain.kindLabel item.fields.kind ++ " · " ++ Domain.categoryLabel item.fields.category) ]
                , Html.h4 [ Attr.class "break-words text-lg font-semibold text-stone-900" ] [ Html.text item.fields.title ]
                , Html.dl [ Attr.class "grid grid-cols-3 gap-2 rounded-lg bg-white p-3 text-center" ]
                    [ score "Facilidad" item.fields.x, score "Cercanía" item.fields.y, score "Potencial" item.fields.z ]
                , Html.p [ Attr.class "whitespace-pre-wrap break-words text-sm leading-relaxed text-stone-600" ] [ Html.text item.fields.notes ]
                , sourceLink item.fields.source
                , if item.fields.proposed then
                    button state (Validate id) "✓ Validar elemento" "w-full rounded-lg border border-emerald-300 bg-emerald-50 px-3 py-2 text-sm text-emerald-800"

                  else
                    Html.p [ Attr.class "text-xs text-emerald-700" ] [ Html.text "Validado" ]
                , button state (Edit id) "Editar calificaciones y datos" (primary ++ " w-full")
                , button state (AskRemove id) "Eliminar" "text-sm text-red-700 underline"
                ]


sourceLink : String -> Html msg
sourceLink source =
    if source == "" then
        Html.text ""

    else
        case Url.fromString source of
            Just url ->
                Html.a [ Attr.href (Url.toString url), Attr.target "_blank", Attr.rel "noopener noreferrer", Attr.class "block text-sm text-emerald-800 underline" ] [ Html.text "Ver correo o fuente ↗" ]

            Nothing ->
                Html.p [ Attr.class "break-words text-sm text-stone-600" ] [ Html.text source ]


score : String -> Rating -> Html msg
score label rating =
    Html.div []
        [ Html.dt [ Attr.class "text-xs text-stone-500" ] [ Html.text label ]
        , Html.dd [ Attr.class "mt-1 text-xl text-stone-800" ] [ Html.text (String.fromInt (Domain.rating rating) ++ "/5") ]
        ]


viewUndo : SaveState -> Board -> Maybe String -> Html Msg
viewUndo state board maybeId =
    case maybeId |> Maybe.andThen (\id -> Dict.get id board.opportunities) of
        Just item ->
            if item.deleted then
                Html.div [ Attr.class "mb-4 flex flex-wrap items-center gap-3 rounded-lg bg-emerald-50 p-3 text-sm text-emerald-900", Attr.attribute "role" "status" ]
                    [ Html.span [] [ Html.text ("«" ++ item.fields.title ++ "» eliminado.") ]
                    , button state Undo "Deshacer" "font-semibold underline"
                    , button state DismissUndo "Cerrar aviso" "ml-auto text-xs underline"
                    ]

            else
                Html.text ""

        Nothing ->
            Html.text ""


viewError : Maybe String -> Html msg
viewError error =
    case error of
        Just message ->
            Html.p [ Attr.class "mb-4 rounded-lg border border-red-200 bg-red-50 p-3 text-sm text-red-800", Attr.attribute "role" "alert" ] [ Html.text message ]

        Nothing ->
            Html.text ""


filterButtons : String -> Maybe a -> (Maybe a -> Msg) -> List ( Maybe a, String ) -> Html Msg
filterButtons label selected toMsg options =
    Html.div [ Attr.class "flex flex-wrap gap-1 rounded-lg bg-stone-100 p-1", Attr.attribute "role" "group", Attr.attribute "aria-label" label ]
        (List.map
            (\( value, title ) ->
                Html.button
                    [ Attr.type_ "button", Events.onClick (toMsg value)
                    , Attr.attribute "aria-pressed" (if selected == value then "true" else "false")
                    , Attr.class (if selected == value then "rounded-md bg-white px-3 py-2 text-xs font-semibold text-emerald-800 shadow-sm" else "rounded-md px-3 py-2 text-xs text-stone-600 hover:bg-white")
                    ]
                    [ Html.text title ]
            )
            options
        )


button : SaveState -> Msg -> String -> String -> Html Msg
button state msg label classes =
    Html.button [ Attr.type_ "button", Attr.disabled (state == Saving), Events.onClick msg, Attr.class (classes ++ " disabled:cursor-wait disabled:opacity-50") ] [ Html.text label ]


field : String -> Html msg -> Html msg
field label input =
    Html.label [ Attr.class "block text-sm text-stone-700" ] [ Html.span [ Attr.class "mb-1 block font-medium" ] [ Html.text label ], input ]


choice : SaveState -> String -> (String -> Msg) -> List ( String, String ) -> Html Msg
choice state current toMsg options =
    Html.select [ Attr.value current, Attr.disabled (state == Saving), Attr.class inputClass, Events.onInput toMsg ]
        (List.map (\( value, label ) -> Html.option [ Attr.value value, Attr.selected (current == value) ] [ Html.text label ]) options)


ratingInput : SaveState -> Axis -> String -> Rating -> Html Msg
ratingInput state axis label rating =
    field label
        (choice state (String.fromInt (Domain.rating rating))
            (\value -> SetRating axis (String.toInt value |> Maybe.withDefault 3 |> Domain.fromRating))
            (List.range 1 5 |> List.map (\number -> ( String.fromInt number, String.fromInt number )))
        )


kindValue : Kind -> String
kindValue kind =
    case kind of
        OpportunityKind ->
            "opportunity"

        Responsibility ->
            "responsibility"


inputClass : String
inputClass =
    "w-full rounded-lg border border-stone-300 bg-white px-3 py-2 text-sm text-stone-900 focus:border-emerald-600 focus:outline-none focus:ring-1 focus:ring-emerald-600 disabled:opacity-60"


primary : String
primary =
    "rounded-lg bg-emerald-800 px-4 py-2 text-sm font-medium text-white hover:bg-emerald-900 disabled:opacity-50"


secondary : String
secondary =
    "rounded-lg border border-stone-300 bg-white px-3 py-2 text-sm text-stone-700 hover:bg-stone-100"


pointDecoder : Decode.Decoder Point
pointDecoder =
    Decode.map2 Point (Decode.field "pageX" Decode.float) (Decode.field "pageY" Decode.float)


startDecoder : String -> Decode.Decoder Msg
startDecoder id =
    Decode.field "button" Decode.int
        |> Decode.andThen
            (\buttonNumber ->
                if buttonNumber == 0 then
                    Decode.map2 (StartDrag id) (Decode.field "pointerId" Decode.int) pointDecoder

                else
                    Decode.fail "Only the primary pointer starts a drag."
            )


mapEvents : SaveState -> List (Html.Attribute Msg)
mapEvents state =
    case state of
        Saving ->
            []

        Ready ->
            [ Events.on "pointermove" (Decode.map2 MovePointer (Decode.field "pointerId" Decode.int) pointDecoder)
            , Events.on "pointerup" (Decode.map2 EndDrag (Decode.field "pointerId" Decode.int) pointDecoder)
            , Events.on "pointercancel" (Decode.succeed CancelDrag)
            , Events.on "pointerleave" (Decode.succeed CancelDrag)
            ]


keyEvent : Decode.Decoder Msg -> Decode.Decoder { message : Msg, stopPropagation : Bool, preventDefault : Bool }
keyEvent decoder =
    Decode.map (\msg -> { message = msg, stopPropagation = True, preventDefault = True }) decoder


escapeDecoder : Decode.Decoder Msg
escapeDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Escape" then
                    Decode.succeed Escape

                else
                    Decode.fail "Not Escape."
            )


moveDecoder : String -> Decode.Decoder Msg
moveDecoder id =
    Decode.map2 Tuple.pair (Decode.field "key" Decode.string) (Decode.field "shiftKey" Decode.bool)
        |> Decode.andThen
            (\( key, shifted ) ->
                let
                    step =
                        if shifted then Fine else Cell

                    direction =
                        case key of
                            "ArrowLeft" -> Just Left
                            "ArrowRight" -> Just Right
                            "ArrowUp" -> Just Up
                            "ArrowDown" -> Just Down
                            _ -> Nothing
                in
                case direction of
                    Just value -> Decode.succeed (MoveKey id value step)
                    Nothing -> Decode.fail "Not an arrow key."
            )
