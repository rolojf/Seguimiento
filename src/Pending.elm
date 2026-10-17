module Pending exposing (Model, Msg, init, isLocal, settle, subscriptions, update, view)

import Bitwise
import Browser.Events
import Dict
import Domain exposing (Board, Change(..), Completion(..), Owner(..), PendingContent(..), PendingFields, PendingMove(..), Ref, Review(..), SaveState(..), Target(..), User)
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Keyed as Keyed
import Json.Decode as Decode


type alias Model =
    { ui : Ui
    , filter : ItemFilter
    , submitting : Submission
    , error : Maybe String
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


type RecordState
    = Active
    | Discarded


type DraftTarget
    = Adding
    | Saved Ref RecordState


type DraftContent
    = TaskDraft String Completion
    | BlankDraft
    | SeparatorDraft String


type alias Draft =
    { target : DraftTarget
    , content : DraftContent
    , owner : Owner
    , shift : String
    , orderRevision : Int
    }



-- The original order and record version remain fixed throughout one drag.


type alias Drag =
    { source : Ref
    , owner : Owner
    , revision : Int
    , ids : List String
    , destination : Maybe String
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
    | Toggle Ref Completion
    | Confirm Ref
    | DiscardRow Ref
    | Filter ItemFilter
    | BeginDrag Drag
    | HoverRow Owner String
    | LeaveRow Owner String
    | LeaveGroup Owner
    | ReleaseDrag
    | CancelDrag


init : Model
init =
    { ui = Browsing, filter = ActiveItems, submitting = Idle, error = Nothing }


primaryButton : Int
primaryButton =
    0


primaryMask : Int
primaryMask =
    1


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.ui of
        Dragging _ ->
            Sub.batch
                [ Browser.Events.onMouseUp (mouseButton ReleaseDrag)
                , Browser.Events.onMouseMove lostButton
                , Browser.Events.onKeyDown escapeKey
                , Browser.Events.onVisibilityChange (\_ -> CancelDrag)
                ]

        _ ->
            Sub.none


isLocal : Msg -> Bool
isLocal msg =
    case msg of
        HoverRow _ _ ->
            True

        LeaveRow _ _ ->
            True

        LeaveGroup _ ->
            True

        CancelDrag ->
            True

        _ ->
            False


update : Msg -> Board -> Model -> ( Model, Maybe Change )
update msg board model =
    case ( msg, model.ui ) of
        ( AddTask, Browsing ) ->
            newDraft (TaskDraft "" Todo) board model

        ( AddSeparator, Browsing ) ->
            newDraft BlankDraft board model

        ( Edit itemId, Browsing ) ->
            case Dict.get itemId board.pending of
                Just item ->
                    ( { model
                        | ui = Editing (draftFrom board item)
                        , error = Nothing
                        , submitting = Idle
                      }
                    , Nothing
                    )

                Nothing ->
                    failure "Este pendiente ya no está disponible." model

        ( ContentChanged content, Editing draft ) ->
            editDraft { draft | content = changeContent content draft.content } model

        ( KindChanged kind, Editing draft ) ->
            editDraft { draft | content = changeKind kind draft.content } model

        ( OwnerChanged owner, Editing draft ) ->
            editDraft { draft | owner = parseOwner owner } model

        ( ShiftChanged shift, Editing draft ) ->
            editDraft { draft | shift = shift } model

        ( Save, Editing draft ) ->
            saveDraft draft model

        ( Retry, Editing draft ) ->
            retryDraft board draft model

        ( Refresh, Editing draft ) ->
            case refreshDraft board draft of
                Ok refreshed ->
                    ( { model | ui = Editing refreshed, error = Nothing }, Nothing )

                Err error ->
                    failure error model

        ( Cancel, Editing _ ) ->
            ( { model | ui = Browsing, error = Nothing, submitting = Idle }, Nothing )

        ( DiscardEdit, Editing draft ) ->
            editorAction Active RemovePending draft model

        ( RestoreEdit, Editing draft ) ->
            editorAction Discarded RestorePending draft model

        ( PurgeEdit, Editing draft ) ->
            editorAction Discarded PurgePending draft model

        ( Toggle reference completion, Browsing ) ->
            ( model, Just (CompletePending reference completion) )

        ( Confirm reference, Browsing ) ->
            ( model, Just (ConfirmPending reference) )

        ( DiscardRow reference, Browsing ) ->
            ( model, Just (RemovePending reference) )

        ( Filter itemFilter, Browsing ) ->
            ( { model | filter = itemFilter, error = Nothing }, Nothing )

        ( BeginDrag drag, Browsing ) ->
            if model.filter == ActiveItems then
                ( { model | ui = Dragging drag, error = Nothing }, Nothing )

            else
                ( model, Nothing )

        ( HoverRow owner itemId, Dragging drag ) ->
            ( { model
                | ui =
                    Dragging
                        { drag
                            | destination =
                                if owner == drag.owner && List.member itemId drag.ids then
                                    Just itemId

                                else
                                    Nothing
                        }
              }
            , Nothing
            )

        ( LeaveGroup owner, Dragging drag ) ->
            if owner == drag.owner then
                ( { model | ui = Dragging { drag | destination = Nothing } }, Nothing )

            else
                ( model, Nothing )

        ( LeaveRow owner itemId, Dragging drag ) ->
            if owner == drag.owner && drag.destination == Just itemId then
                ( { model | ui = Dragging { drag | destination = Nothing } }, Nothing )

            else
                ( model, Nothing )

        ( ReleaseDrag, Dragging drag ) ->
            releaseDrag drag model

        ( CancelDrag, Dragging _ ) ->
            ( { model | ui = Browsing }, Nothing )

        _ ->
            ( model, Nothing )


newDraft : DraftContent -> Board -> Model -> ( Model, Maybe Change )
newDraft content board model =
    ( { model
        | ui = Editing { target = Adding, content = content, owner = Rolando, shift = "0", orderRevision = board.pendingOrderRevision }
        , filter = ActiveItems
        , error = Nothing
        , submitting = Idle
      }
    , Nothing
    )


draftFrom : Board -> Domain.Pending -> Draft
draftFrom board item =
    { target =
        Saved (Domain.ref item)
            (if item.deleted then
                Discarded

             else
                Active
            )
    , content =
        case item.fields.content of
            PendingTask title completion ->
                TaskDraft title completion

            Separator "" ->
                BlankDraft

            Separator content ->
                SeparatorDraft content
    , owner = item.fields.owner
    , shift = "0"
    , orderRevision = board.pendingOrderRevision
    }


editDraft : Draft -> Model -> ( Model, Maybe Change )
editDraft draft model =
    if isDiscarded draft then
        ( model, Nothing )

    else
        ( { model | ui = Editing draft }, Nothing )


changeContent : String -> DraftContent -> DraftContent
changeContent content current =
    case current of
        TaskDraft _ completion ->
            TaskDraft content completion

        BlankDraft ->
            BlankDraft

        SeparatorDraft _ ->
            SeparatorDraft content


changeKind : String -> DraftContent -> DraftContent
changeKind kind current =
    if kind == contentKind current then
        current

    else
        case kind of
            "blank" ->
                BlankDraft

            "separator" ->
                SeparatorDraft "- - -"

            _ ->
                TaskDraft "" Todo


contentKind : DraftContent -> String
contentKind content =
    case content of
        TaskDraft _ _ ->
            "task"

        BlankDraft ->
            "blank"

        SeparatorDraft _ ->
            "separator"


saveDraft : Draft -> Model -> ( Model, Maybe Change )
saveDraft draft model =
    if isDiscarded draft then
        failure "Restaura este pendiente antes de modificarlo." model

    else
        case String.toInt (String.trim draft.shift) of
            Nothing ->
                failure "El movimiento debe ser un número entero: positivo sube y negativo baja." model

            Just steps ->
                let
                    fields =
                        draftFields draft

                    move =
                        if steps == 0 then
                            KeepPosition

                        else
                            ShiftPosition { revision = draft.orderRevision, steps = steps }
                in
                if Domain.pendingCompletion fields.content /= Nothing && String.isEmpty (String.trim (Domain.pendingTitle fields.content)) then
                    failure "Escribe el texto del pendiente." model

                else
                    ( { model | ui = Editing draft, submitting = SaveChange }
                    , Just (SavePending (domainTarget draft.target) fields move)
                    )


retryDraft : Board -> Draft -> Model -> ( Model, Maybe Change )
retryDraft board draft model =
    case refreshDraft board draft of
        Err error ->
            failure error model

        Ok refreshed ->
            if isDiscarded refreshed then
                failure "El pendiente fue descartado. Restaura para modificarlo." { model | ui = Editing refreshed }

            else
                saveDraft refreshed model


refreshDraft : Board -> Draft -> Result String Draft
refreshDraft board draft =
    case draft.target of
        Adding ->
            Ok { draft | orderRevision = board.pendingOrderRevision }

        Saved reference _ ->
            case Dict.get reference.id board.pending of
                Nothing ->
                    Err "El pendiente fue borrado definitivamente. Conservamos tu texto; puedes copiarlo antes de cerrar."

                Just current ->
                    Ok
                        { draft
                            | target =
                                Saved (Domain.ref current)
                                    (if current.deleted then
                                        Discarded

                                     else
                                        Active
                                    )
                            , orderRevision = board.pendingOrderRevision
                            , content =
                                case ( draft.content, current.fields.content ) of
                                    ( TaskDraft title _, PendingTask _ completion ) ->
                                        TaskDraft title completion

                                    _ ->
                                        draft.content
                        }


editorAction : RecordState -> (Ref -> Change) -> Draft -> Model -> ( Model, Maybe Change )
editorAction expected action draft model =
    case draft.target of
        Saved reference state ->
            if state == expected then
                ( { model | submitting = StatusChange }, Just (action reference) )

            else
                ( model, Nothing )

        Adding ->
            ( model, Nothing )


domainTarget : DraftTarget -> Target
domainTarget target =
    case target of
        Adding ->
            New

        Saved reference _ ->
            Existing reference


draftFields : Draft -> PendingFields
draftFields draft =
    { content =
        case draft.content of
            TaskDraft title completion ->
                PendingTask title completion

            BlankDraft ->
                Separator ""

            SeparatorDraft content ->
                Separator content
    , owner = draft.owner
    }


isDiscarded : Draft -> Bool
isDiscarded draft =
    case draft.target of
        Saved _ Discarded ->
            True

        _ ->
            False


parseOwner : String -> Owner
parseOwner owner =
    case owner of
        "dot" ->
            Dot

        "Otros" ->
            Others

        _ ->
            Rolando


failure : String -> Model -> ( Model, Maybe Change )
failure error model =
    ( { model | error = Just error, submitting = Idle }, Nothing )


settle : Result String () -> Model -> Model
settle result model =
    case result of
        Ok _ ->
            { model
                | ui =
                    if model.submitting /= Idle then
                        Browsing

                    else
                        model.ui
                , submitting = Idle
                , error = Nothing
            }

        Err error ->
            -- Retain which editor action failed; restoring must never retry as a save.
            { model | error = Just error }


releaseDrag : Drag -> Model -> ( Model, Maybe Change )
releaseDrag drag model =
    let
        steps =
            Maybe.map2 (-) (indexOf drag.source.id drag.ids) (drag.destination |> Maybe.andThen (\itemId -> indexOf itemId drag.ids))
    in
    ( { model | ui = Browsing }
    , steps
        |> Maybe.andThen
            (\amount ->
                if amount == 0 then
                    Nothing

                else
                    Just (MovePending drag.source { revision = drag.revision, steps = amount })
            )
    )


indexOf : String -> List String -> Maybe Int
indexOf itemId ids =
    ids |> List.indexedMap Tuple.pair |> List.filter (\( _, current ) -> current == itemId) |> List.head |> Maybe.map Tuple.first


mouseButton : Msg -> Decode.Decoder Msg
mouseButton msg =
    Decode.field "button" Decode.int
        |> Decode.andThen
            (\buttonCode ->
                if buttonCode == primaryButton then
                    Decode.succeed msg

                else
                    Decode.fail "Only the primary mouse button starts or ends this drag."
            )


lostButton : Decode.Decoder Msg
lostButton =
    Decode.field "buttons" Decode.int
        |> Decode.andThen
            (\buttons ->
                if Bitwise.and buttons primaryMask == 0 then
                    Decode.succeed CancelDrag

                else
                    Decode.fail "The primary mouse button is still pressed."
            )


escapeKey : Decode.Decoder Msg
escapeKey =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Escape" then
                    Decode.succeed CancelDrag

                else
                    Decode.fail "Other key"
            )


view : SaveState -> User -> Board -> Model -> Html Msg
view state user board model =
    let
        items =
            Dict.values board.pending

        discarded =
            List.filter .deleted items |> List.sortBy (\item -> ( item.updatedAt, item.id )) |> List.reverse

        count =
            List.filter (\item -> not item.deleted && Domain.pendingCompletion item.fields.content == Just Todo) items |> List.length

        locked =
            if model.ui == Browsing then
                state

            else
                Saving
    in
    -- section-pending supplies the section accent; neutral surfaces follow the global theme.
    Html.section [ A.id "pendientes", A.class "section-pending rounded-xl border border-slate-200 bg-white p-5 sm:p-7" ]
        [ Html.div [ A.class "mb-5 flex flex-wrap items-center justify-between gap-3" ]
            [ Html.div []
                [ Html.h2 [ A.class "text-xl font-semibold text-slate-900" ] [ Html.text "Pendientes" ]
                , Html.p [ A.class "text-base text-slate-500" ]
                    [ Html.text
                        (if model.filter == ActiveItems then
                            String.fromInt count ++ " por resolver"

                         else
                            String.fromInt (List.length discarded) ++ " descartados · más recientes primero"
                        )
                    ]
                ]
            , Html.div [ A.class "flex flex-wrap gap-2" ]
                [ button locked AddTask "Añadir pendiente", button locked AddSeparator "Añadir separador" ]
            ]
        , filterView locked model.filter
        , case model.error of
            Just error ->
                Html.p [ A.class "mb-4 rounded border border-red-200 bg-red-50 p-3 text-base text-red-800", A.attribute "role" "alert" ] [ Html.text error ]

            Nothing ->
                Html.text ""
        , case model.ui of
            Editing draft ->
                editorView state model.error model.submitting draft

            Dragging _ ->
                Html.div [ A.class "mb-4 flex flex-wrap items-center gap-3 text-base", A.attribute "role" "status" ]
                    [ Html.text "Suelta sobre otro elemento del mismo responsable. Fuera del grupo o Escape cancela."
                    , button Ready CancelDrag "Cancelar arrastre"
                    ]

            Browsing ->
                Html.text ""
        , case model.filter of
            ActiveItems ->
                Html.div [ A.class "grid gap-5 lg:grid-cols-3" ]
                    (List.map (groupView locked user board model) [ Rolando, Dot, Others ])

            DiscardedItems ->
                if List.isEmpty discarded then
                    Html.p [ A.class "text-base text-slate-600" ] [ Html.text "No hay pendientes descartados." ]

                else
                    Keyed.node "ul" [ A.class "grid gap-3 md:grid-cols-2" ] (List.map (\item -> ( item.id, discardedRow locked item )) discarded)
        ]


filterView : SaveState -> ItemFilter -> Html Msg
filterView state current =
    Html.div [ A.class "mb-5 flex flex-wrap gap-2", A.attribute "role" "group", A.attribute "aria-label" "Estado de pendientes" ]
        (List.map
            (\( filter, title ) ->
                Html.button
                    [ A.type_ "button"
                    , E.onClick (Filter filter)
                    , A.disabled (state == Saving)
                    , A.attribute "aria-pressed"
                        (if current == filter then
                            "true"

                         else
                            "false"
                        )
                    , A.class
                        (if current == filter then
                            "rounded-lg bg-slate-900 px-3 py-2 text-base text-white"

                         else
                            "rounded-lg border border-slate-300 px-3 py-2 text-base text-slate-700"
                        )
                    ]
                    [ Html.text title ]
            )
            [ ( ActiveItems, "Activos" ), ( DiscardedItems, "Descartados" ) ]
        )


groupView : SaveState -> User -> Board -> Model -> Owner -> Html Msg
groupView state user board model owner =
    let
        items =
            Domain.orderedPending owner board

        ids =
            List.map .id items
    in
    Html.div [ A.class "min-w-0 rounded-lg bg-slate-50 p-4", E.onMouseLeave (LeaveGroup owner) ]
        [ Html.h3 [ A.class "mb-3 font-semibold text-slate-700" ] [ Html.text (Domain.ownerLabel owner) ]
        , if List.isEmpty items then
            Html.p [ A.class "text-base text-slate-500" ] [ Html.text "Sin pendientes" ]

          else
            Keyed.node "ul"
                [ A.class "space-y-2" ]
                (List.map (\item -> ( item.id, activeRow state user board.pendingOrderRevision ids model.ui item )) items)
        ]


activeRow : SaveState -> User -> Int -> List String -> Ui -> Domain.Pending -> Html Msg
activeRow state user revision ids ui item =
    Html.li
        [ A.class ("rounded px-1 py-2 " ++ dragClass ui item.id)
        , E.onMouseEnter (HoverRow item.fields.owner item.id)
        , E.onMouseLeave (LeaveRow item.fields.owner item.id)
        ]
        [ Html.div [ A.class "flex items-start gap-2" ]
            [ dragHandle state revision ids item
            , Html.div [ A.class "min-w-0 flex-1 space-y-2" ]
                [ contentView state item
                , if item.review == NeedsReview user then
                    Html.div [ A.class "flex flex-wrap items-center gap-2" ]
                        [ Html.span [ A.class "font-bold text-amber-700", A.attribute "role" "img", A.attribute "aria-label" "Elemento nuevo por revisar" ] [ Html.text "!" ]
                        , button state (Confirm (Domain.ref item)) "Confirmar"
                        , button state (DiscardRow (Domain.ref item)) "Descartar"
                        ]

                  else
                    Html.text ""
                ]
            , Html.button
                [ A.type_ "button"
                , E.onClick (Edit item.id)
                , A.disabled (state == Saving)
                , A.class "shrink-0 rounded border border-slate-300 bg-white px-2 py-1 text-base font-medium text-slate-700 hover:bg-slate-50 disabled:opacity-50"
                , A.attribute "aria-label" ("Editar " ++ itemLabel item)
                ]
                [ Html.text "Editar" ]
            ]
        ]


contentView : SaveState -> Domain.Pending -> Html Msg
contentView state item =
    case item.fields.content of
        PendingTask title completion ->
            Html.label [ A.class "flex min-w-0 items-start gap-2 text-base" ]
                [ Html.input
                    [ A.type_ "checkbox"
                    , A.checked (completion == Done)
                    , A.disabled (state == Saving)
                    , A.class "mt-1 h-4 w-4 shrink-0"
                    , E.onCheck
                        (\done ->
                            Toggle (Domain.ref item)
                                (if done then
                                    Done

                                 else
                                    Todo
                                )
                        )
                    ]
                    []
                , Html.span
                    [ A.classList [ ( "min-w-0 break-words", True ), ( "text-slate-400 line-through", completion == Done ) ]
                    , A.style "overflow-wrap" "anywhere"
                    ]
                    [ Html.text title ]
                ]

        Separator content ->
            Html.div
                [ A.class "min-h-8 whitespace-pre-wrap break-words text-center text-base text-slate-600"
                , A.style "overflow-wrap" "anywhere"
                , A.attribute "aria-label"
                    (if String.isEmpty content then
                        "Separador vacío"

                     else
                        "Separador: " ++ content
                    )
                ]
                [ Html.text content ]


dragHandle : SaveState -> Int -> List String -> Domain.Pending -> Html Msg
dragHandle state revision ids item =
    let
        drag =
            { source = Domain.ref item, owner = item.fields.owner, revision = revision, ids = ids, destination = Just item.id }
    in
    Html.button
        [ A.type_ "button"
        , A.disabled (state == Saving)
        , A.class "shrink-0 cursor-grab rounded px-1 text-base text-slate-500 active:cursor-grabbing"
        , A.attribute "aria-label" ("Arrastrar " ++ itemLabel item ++ ". También puedes cambiar su posición desde Editar.")
        , E.preventDefaultOn "mousedown" (Decode.map (\msg -> ( msg, True )) (mouseButton (BeginDrag drag)))
        ]
        [ Html.text "↕" ]


dragClass : Ui -> String -> String
dragClass ui itemId =
    case ui of
        Dragging drag ->
            if itemId == drag.source.id then
                "border-y-2 border-transparent opacity-50"

            else if drag.destination == Just itemId then
                case Maybe.map2 compare (indexOf drag.source.id drag.ids) (indexOf itemId drag.ids) of
                    Just LT ->
                        "border-y-2 border-t-transparent border-b-blue-600"

                    Just GT ->
                        "border-y-2 border-b-transparent border-t-blue-600"

                    _ ->
                        "border-y-2 border-transparent"

            else
                "border-y-2 border-transparent"

        _ ->
            "border-y-2 border-transparent"


discardedRow : SaveState -> Domain.Pending -> Html Msg
discardedRow state item =
    Html.li [ A.class "min-w-0 space-y-2 rounded border border-slate-200 bg-white p-3" ]
        [ Html.p [ A.class "break-words text-base font-medium text-slate-700", A.style "overflow-wrap" "anywhere" ] [ Html.text (itemLabel item) ]
        , Html.p [ A.class "text-sm text-slate-600" ] [ Html.text (Domain.ownerLabel item.fields.owner ++ " · Descartado") ]
        , Html.time [ A.datetime item.updatedAt, A.class "block text-sm text-slate-600" ]
            [ Html.text ((String.left 19 item.updatedAt |> String.replace "T" " ") ++ " UTC") ]
        , button state (Edit item.id) "Editar"
        ]


itemLabel : Domain.Pending -> String
itemLabel item =
    case item.fields.content of
        PendingTask title _ ->
            title

        Separator "" ->
            "Separador vacío"

        Separator content ->
            content


editorView : SaveState -> Maybe String -> Submission -> Draft -> Html Msg
editorView state error submission draft =
    let
        fieldState =
            if isDiscarded draft then
                Saving

            else
                state
    in
    Html.form [ E.onSubmit Save, A.class "mb-6 space-y-4 rounded-lg border border-slate-200 p-4" ]
        [ Html.h3 [ A.class "text-lg font-semibold" ]
            [ Html.text
                (if draft.target == Adding then
                    "Nuevo elemento"

                 else
                    "Editar elemento"
                )
            ]
        , Html.div [ A.class "grid gap-4 sm:grid-cols-2" ]
            [ field "pending-kind"
                "Tipo"
                (Html.select [ A.id "pending-kind", A.class inputClass, A.value (contentKind draft.content), A.disabled (fieldState == Saving), E.onInput KindChanged ]
                    [ Html.option [ A.value "task" ] [ Html.text "Pendiente" ]
                    , Html.option [ A.value "blank" ] [ Html.text "Separador vacío" ]
                    , Html.option [ A.value "separator" ] [ Html.text "Separador con texto" ]
                    ]
                )
            , field "pending-owner"
                "Responsable"
                (Html.select [ A.id "pending-owner", A.class inputClass, A.value (Domain.ownerLabel draft.owner), A.disabled (fieldState == Saving), E.onInput OwnerChanged ]
                    (List.map (\owner -> Html.option [ A.value (Domain.ownerLabel owner) ] [ Html.text (Domain.ownerLabel owner) ]) [ Rolando, Dot, Others ])
                )
            ]
        , case draft.content of
            TaskDraft title _ ->
                field "pending-content"
                    "Pendiente"
                    (Html.input [ A.id "pending-content", A.class inputClass, A.value title, A.required True, A.maxlength 90, A.disabled (fieldState == Saving), E.onInput ContentChanged ] [])

            BlankDraft ->
                Html.p [ A.class "text-sm text-slate-600" ] [ Html.text "Se dejará un espacio visible dentro del grupo." ]

            SeparatorDraft content ->
                field "pending-content"
                    "Texto del separador"
                    (Html.input [ A.id "pending-content", A.class inputClass, A.value content, A.placeholder "- - -", A.maxlength 90, A.disabled (fieldState == Saving), E.onInput ContentChanged ] [])
        , if isDiscarded draft then
            Html.p [ A.class "text-base text-slate-600" ] [ Html.text "Restaura para modificar este pendiente. Borrar definitivamente lo elimina del tablero y de sus próximos respaldos." ]

          else
            Html.div [ A.class "space-y-1" ]
                [ field "pending-shift"
                    "Mover posiciones"
                    (Html.input
                        [ A.id "pending-shift"
                        , A.type_ "number"
                        , A.step "1"
                        , A.required True
                        , A.class inputClass
                        , A.value draft.shift
                        , A.disabled (state == Saving)
                        , E.onInput ShiftChanged
                        , A.attribute "aria-describedby" "pending-shift-hint"
                        ]
                        []
                    )
                , Html.p [ A.id "pending-shift-hint", A.class "text-sm text-slate-600" ] [ Html.text "Positivo sube; negativo baja; 0 conserva la posición. El movimiento se guarda junto con los campos dentro del responsable elegido." ]
                ]
        , Html.div [ A.class "flex flex-wrap gap-2" ]
            (editorButtons state draft
                ++ [ button state Cancel "Cerrar" ]
            )
        , if error /= Nothing && (isDiscarded draft || submission == StatusChange) then
            Html.div [ A.class "space-y-2 text-sm text-slate-600" ]
                [ Html.p [] [ Html.text "Actualiza la versión antes de volver a descartar, restaurar o borrar. Este botón no modifica el registro." ]
                , button state Refresh "Actualizar versión"
                ]

          else if error /= Nothing then
            Html.div [ A.class "space-y-2 text-sm text-slate-600" ]
                [ Html.p [] [ Html.text "Tu texto y movimiento siguen aquí. Revisa la lista antes de aplicar ambos sobre la versión actual." ]
                , button state Retry "Aplicar sobre versión actual"
                ]

          else
            Html.text ""
        ]


editorButtons : SaveState -> Draft -> List (Html Msg)
editorButtons state draft =
    case draft.target of
        Saved _ Discarded ->
            [ button state RestoreEdit "Restaurar"
            , Html.button [ A.type_ "button", A.disabled (state == Saving), E.onClick PurgeEdit, A.class "rounded border border-red-300 px-3 py-2 text-base font-medium text-red-700 disabled:opacity-50" ] [ Html.text "Borrar definitivamente" ]
            ]

        _ ->
            [ Html.button [ A.type_ "submit", A.disabled (state == Saving), A.class "rounded bg-slate-900 px-4 py-2 text-base font-medium text-white disabled:opacity-50" ] [ Html.text "Guardar" ]
            ]
                ++ (case draft.target of
                        Saved _ Active ->
                            [ button state DiscardEdit "Descartar" ]

                        _ ->
                            []
                   )


field : String -> String -> Html Msg -> Html Msg
field controlId title control =
    Html.div [ A.class "min-w-0 space-y-1" ]
        [ Html.label [ A.for controlId, A.class "block text-base font-medium" ] [ Html.text title ]
        , control
        ]


button : SaveState -> Msg -> String -> Html Msg
button state msg title =
    Html.button [ A.type_ "button", E.onClick msg, A.disabled (state == Saving), A.class "rounded border border-slate-300 px-3 py-2 text-base font-medium hover:bg-slate-50 disabled:opacity-50" ] [ Html.text title ]


inputClass : String
inputClass =
    "block w-full min-w-0 rounded border border-slate-300 bg-white px-3 py-2 text-base text-slate-900 disabled:opacity-60"
