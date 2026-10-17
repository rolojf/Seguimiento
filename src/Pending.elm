module Pending exposing (Model, Msg, init, isLocal, isReview, isWaiting, settle, subscriptions, update, view)

import Bitwise
import Browser.Dom
import Browser.Events
import Dict
import Domain exposing (Board, Change(..), Completion(..), CompletionEdit(..), Owner(..), PendingContent(..), PendingFields, PendingMove(..), PendingStatus(..), Ref, Review(..), SaveState(..), Target(..), User)
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Keyed as Keyed
import Json.Decode as Decode
import Process
import Task


type alias Model =
    { ui : Ui
    , filter : ItemFilter
    , submitting : Submission
    , error : Maybe String
    , nextReview : Int
    }


type Ui
    = Browsing
    | Editing Draft
    | Dragging Drag
    | Reviewing ReviewWait


type alias ReviewWait =
    { ref : Ref, token : Int, deadline : Float }


type ItemFilter
    = ActiveItems
    | CompletedItems
    | DiscardedItems


type Submission
    = Idle
    | SaveChange
    | StatusChange


type RecordState
    = Active
    | Finished
    | Discarded


type DraftTarget
    = Adding
    | Saved Ref RecordState


type DraftContent
    = TaskDraft String PendingStatus
    | BlankDraft
    | SeparatorDraft String


type alias Draft =
    { target : DraftTarget
    , content : DraftContent
    , owner : Owner
    , shift : String
    , orderRevision : Int
    , completion : CompletionEdit
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
    | CompletionChanged String
    | Save
    | Retry
    | Refresh
    | Cancel
    | DiscardEdit
    | RestoreEdit
    | PurgeEdit
    | Toggle Ref
    | ReviewClick Ref Float
    | ReviewElapsed ReviewWait
    | Confirm Ref
    | DiscardRow Ref
    | PurgeLast Owner Ref
    | DeleteSeparator Ref
    | Filter ItemFilter
    | BeginDrag Drag
    | HoverRow Owner String
    | LeaveRow Owner String
    | LeaveGroup Owner
    | ReleaseDrag
    | CancelDrag
    | FocusedInput


init : Model
init =
    { ui = Browsing, filter = ActiveItems, submitting = Idle, error = Nothing, nextReview = 0 }


reviewDelay : Float
reviewDelay =
    3000


isWaiting : Model -> Bool
isWaiting model =
    case model.ui of
        Reviewing _ ->
            True

        _ ->
            False


isReview : Msg -> Bool
isReview msg =
    case msg of
        ReviewClick _ _ ->
            True

        ReviewElapsed _ ->
            True

        _ ->
            False


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

        FocusedInput ->
            True

        _ ->
            False


update : User -> Msg -> Board -> Model -> ( Model, Cmd Msg, Maybe Change )
update user msg board model =
    case ( msg, model.ui ) of
        ( AddTask, Browsing ) ->
            let
                ( updated, change ) =
                    newDraft (TaskDraft "" Approved) board model
            in
            ( updated, Browser.Dom.focus "pending-content" |> Task.attempt (\_ -> FocusedInput), change )

        ( ReviewClick reference clickedAt, Browsing ) ->
            if canReview user reference board then
                let
                    waiting =
                        { ref = reference, token = model.nextReview, deadline = clickedAt + reviewDelay }
                in
                ( { model
                    | ui = Reviewing waiting
                    , nextReview = model.nextReview + 1
                    , error = Nothing
                  }
                , Process.sleep reviewDelay |> Task.perform (\_ -> ReviewElapsed waiting)
                , Nothing
                )

            else
                ( model, Cmd.none, Nothing )

        ( ReviewClick reference clickedAt, Reviewing waiting ) ->
            if reference == waiting.ref then
                finishReview user
                    board
                    waiting
                    (if clickedAt < waiting.deadline then
                        RejectPendingCompletion reference

                     else
                        TogglePending reference
                    )
                    model

            else
                ( model, Cmd.none, Nothing )

        ( ReviewElapsed elapsed, Reviewing waiting ) ->
            -- Include the click deadline so stale timers cannot survive a new login.
            if elapsed == waiting then
                finishReview user board waiting (TogglePending waiting.ref) model

            else
                ( model, Cmd.none, Nothing )

        _ ->
            let
                ( updated, change ) =
                    updateReady msg board model
            in
            ( updated, Cmd.none, change )


canReview : User -> Ref -> Board -> Bool
canReview user reference board =
    Dict.get reference.id board.pending
        |> Maybe.map
            (\item ->
                Domain.ref item
                    == reference
                    && not item.deleted
                    && Domain.pendingReview item.fields.content
                    == NeedsReview user
            )
        |> Maybe.withDefault False


finishReview : User -> Board -> ReviewWait -> Change -> Model -> ( Model, Cmd Msg, Maybe Change )
finishReview user board waiting change model =
    if canReview user waiting.ref board then
        ( { model | ui = Browsing, error = Nothing }, Cmd.none, Just change )

    else
        ( { model | ui = Browsing, error = Just "El pendiente cambió durante la espera. Revisa su estado actual." }, Cmd.none, Nothing )


updateReady : Msg -> Board -> Model -> ( Model, Maybe Change )
updateReady msg board model =
    case ( msg, model.ui ) of
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
            editDraft { draft | content = changeKind kind draft.content, completion = KeepCompletion } model

        ( OwnerChanged owner, Editing draft ) ->
            editDraft { draft | owner = parseOwner owner } model

        ( ShiftChanged shift, Editing draft ) ->
            editDraft { draft | shift = shift } model

        ( CompletionChanged value, Editing draft ) ->
            editDraft { draft | completion = selectedCompletion value draft, shift = "0" } model

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

        ( Toggle reference, Browsing ) ->
            ( model, Just (TogglePending reference) )

        ( Confirm reference, Browsing ) ->
            ( model, Just (ConfirmPending reference) )

        ( DiscardRow reference, Browsing ) ->
            ( model, Just (RemovePending reference) )

        ( PurgeLast owner reference, Browsing ) ->
            ( model, Just (PurgeCompleted owner reference) )

        ( DeleteSeparator reference, Browsing ) ->
            ( model, Just (PurgeSeparator reference) )

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
        | ui = Editing { target = Adding, content = content, owner = Rolando, shift = "0", orderRevision = board.pendingOrderRevision, completion = KeepCompletion }
        , filter = ActiveItems
        , error = Nothing
        , submitting = Idle
      }
    , Nothing
    )


draftFrom : Board -> Domain.Pending -> Draft
draftFrom board item =
    { target =
        Saved (Domain.ref item) (recordState item)
    , content =
        case item.fields.content of
            PendingTask title status ->
                TaskDraft title status

            Separator "" ->
                BlankDraft

            Separator content ->
                SeparatorDraft content
    , owner = item.fields.owner
    , shift = "0"
    , orderRevision = board.pendingOrderRevision
    , completion = KeepCompletion
    }


editDraft : Draft -> Model -> ( Model, Maybe Change )
editDraft draft model =
    if isReadOnly draft then
        ( model, Nothing )

    else
        ( { model | ui = Editing draft }, Nothing )


changeContent : String -> DraftContent -> DraftContent
changeContent content current =
    case current of
        TaskDraft _ status ->
            TaskDraft content status

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
                TaskDraft "" Approved


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
    if isReadOnly draft then
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
                    , Just (SavePending (domainTarget draft.target) fields move draft.completion)
                    )


retryDraft : Board -> Draft -> Model -> ( Model, Maybe Change )
retryDraft board draft model =
    case refreshDraft board draft of
        Err error ->
            failure error model

        Ok refreshed ->
            if refreshed.owner /= draft.owner then
                failure "La tarea se completó en otra sesión. Conservamos tu texto y recuperamos su responsable actual; guarda Pendiente antes de cambiarlo." { model | ui = Editing refreshed }

            else if isReadOnly refreshed then
                failure "El pendiente fue descartado. Restaura antes de modificarlo." { model | ui = Editing refreshed }

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
                                Saved (Domain.ref current) (recordState current)
                            , orderRevision = board.pendingOrderRevision
                            , owner =
                                if Domain.pendingCompletion current.fields.content == Just Done then
                                    current.fields.owner

                                else
                                    draft.owner
                            , content =
                                case ( draft.content, current.fields.content ) of
                                    ( TaskDraft title _, PendingTask _ status ) ->
                                        TaskDraft title status

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
            TaskDraft title status ->
                PendingTask title status

            BlankDraft ->
                Separator ""

            SeparatorDraft content ->
                Separator content
    , owner = draft.owner
    }


draftCompletion : Draft -> Completion
draftCompletion draft =
    case draft.completion of
        KeepCompletion ->
            Domain.pendingCompletion (draftFields draft).content |> Maybe.withDefault Todo

        ChangeCompletion completion ->
            completion


selectedCompletion : String -> Draft -> CompletionEdit
selectedCompletion value draft =
    let
        selection =
            case value of
                "pending" ->
                    Just Todo

                "completed" ->
                    Just Done

                _ ->
                    Nothing
    in
    case selection of
        Just completion ->
            if Domain.pendingCompletion (draftFields draft).content == Just completion then
                KeepCompletion

            else
                ChangeCompletion completion

        Nothing ->
            draft.completion


isReadOnly : Draft -> Bool
isReadOnly draft =
    case draft.target of
        Saved _ Discarded ->
            True

        _ ->
            False


recordState : Domain.Pending -> RecordState
recordState item =
    if item.deleted then
        Discarded

    else if Domain.pendingIsComplete item.fields.content then
        Finished

    else
        Active


parseOwner : String -> Owner
parseOwner owner =
    case owner of
        "Dot" ->
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

        completedCount =
            List.filter (\item -> not item.deleted && Domain.pendingIsComplete item.fields.content) items |> List.length

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
                        (case model.filter of
                            ActiveItems ->
                                String.fromInt count ++ " por resolver"

                            CompletedItems ->
                                String.fromInt completedCount ++ " completados · más recientes primero"

                            DiscardedItems ->
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

            Reviewing _ ->
                Html.span [ A.class "sr-only", A.attribute "role" "status" ]
                    [ Html.text "Esperando tres segundos. Otro clic deja la tarea pendiente; sin otro clic pasa a Completados." ]

            Browsing ->
                Html.text ""
        , case model.filter of
            ActiveItems ->
                Html.div [ A.class "grid gap-5 lg:grid-cols-3" ]
                    (List.map (groupView state user board model) [ Rolando, Dot, Others ])

            CompletedItems ->
                Html.div [ A.class "grid gap-5 lg:grid-cols-3" ]
                    (List.map (completedGroup locked user board) [ Rolando, Dot, Others ])

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
            [ ( ActiveItems, "Activos" ), ( CompletedItems, "Completados" ), ( DiscardedItems, "Descartados" ) ]
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


completedGroup : SaveState -> User -> Board -> Owner -> Html Msg
completedGroup state user board owner =
    let
        items =
            Domain.completedPending owner board
    in
    Html.div [ A.class "min-w-0 rounded-lg bg-slate-50 p-4" ]
        [ Html.div [ A.class "mb-3 flex items-center gap-2" ]
            [ Html.h3 [ A.class "font-semibold text-slate-700" ] [ Html.text (Domain.ownerLabel owner) ]
            , case List.reverse items |> List.head of
                Just oldest ->
                    reviewButton state
                        (PurgeLast owner (Domain.ref oldest))
                        ("Borrar definitivamente el completado más antiguo de " ++ Domain.ownerLabel owner ++ ": " ++ itemLabel oldest)
                        "✕"

                Nothing ->
                    Html.text ""
            ]
        , if List.isEmpty items then
            Html.p [ A.class "text-base text-slate-500" ] [ Html.text "Sin completados" ]

          else
            Keyed.node "ul"
                [ A.class "space-y-3" ]
                (List.map
                    (\item ->
                        ( item.id
                        , Html.li [ A.class "space-y-1" ]
                            [ Html.div [ A.class "flex items-start gap-2" ]
                                [ Html.div [ A.class "min-w-0 flex-1" ] [ contentView state user Browsing item ]
                                , editButton state item
                                ]
                            , Html.time [ A.datetime item.updatedAt, A.class "block text-[0.625rem] leading-tight text-slate-600" ]
                                [ Html.text ((String.left 19 item.updatedAt |> String.replace "T" " ") ++ " UTC") ]
                            ]
                        )
                    )
                    items
                )
        ]


activeRow : SaveState -> User -> Int -> List String -> Ui -> Domain.Pending -> Html Msg
activeRow state user revision ids ui item =
    let
        locked =
            if ui == Browsing then
                state

            else
                Saving

        checkboxState =
            case ui of
                Reviewing waiting ->
                    if waiting.ref == Domain.ref item then
                        state

                    else
                        locked

                _ ->
                    locked
    in
    Html.li
        [ A.class ("rounded px-1 py-2 " ++ dragClass ui item.id)
        , E.onMouseEnter (HoverRow item.fields.owner item.id)
        , E.onMouseLeave (LeaveRow item.fields.owner item.id)
        ]
        [ Html.div [ A.class "flex items-start gap-2" ]
            [ dragHandle locked revision ids item
            , Html.div [ A.class "min-w-0 flex-1 space-y-2" ]
                [ contentView checkboxState user ui item
                , if Domain.pendingApproval item.fields.content == Just user then
                    Html.div [ A.class "flex flex-wrap items-center gap-2" ]
                        [ Html.span [ A.class "font-bold text-amber-700", A.attribute "role" "img", A.attribute "aria-label" "Elemento nuevo por revisar" ] [ Html.text "!" ]
                        , reviewButton locked (Confirm (Domain.ref item)) "Confirmar pendiente" "✓"
                        , reviewButton locked (DiscardRow (Domain.ref item)) "Descartar pendiente" "✕"
                        ]

                  else
                    Html.text ""
                ]
            , itemActions locked item
            ]
        ]


contentView : SaveState -> User -> Ui -> Domain.Pending -> Html Msg
contentView state user ui item =
    case item.fields.content of
        PendingTask title _ ->
            let
                reviewNeeded =
                    Domain.pendingReview item.fields.content == NeedsReview user

                complete =
                    Domain.pendingCompletion item.fields.content == Just Done

                interaction =
                    if reviewNeeded then
                        E.preventDefaultOn "click"
                            (Decode.field "timeStamp" Decode.float
                                |> Decode.map (\time -> ( ReviewClick (Domain.ref item) time, True ))
                            )

                    else
                        E.onCheck (\_ -> Toggle (Domain.ref item))

                description =
                    if reviewNeeded then
                        title ++ ". Completado por el otro usuario. Un clic espera tres segundos y confirma; otro clic dentro de esos tres segundos lo deja pendiente."

                    else if Domain.pendingIsComplete item.fields.content then
                        title ++ ". Desmarca para reabrir este pendiente."

                    else
                        title
            in
            Html.label [ A.class "flex min-w-0 items-start gap-2 text-base" ]
                [ Html.span [ A.class "mt-1 inline-flex shrink-0 items-center gap-0.5" ]
                    [ Html.input
                        [ A.type_ "checkbox"
                        , A.checked complete
                        , A.disabled (state == Saving || Domain.pendingApproval item.fields.content /= Nothing)
                        , A.classList
                            [ ( "h-4 w-4 shrink-0", True )
                            , ( "ring-2 ring-amber-500 ring-offset-1"
                              , case ui of
                                    Reviewing waiting ->
                                        waiting.ref == Domain.ref item

                                    _ ->
                                        False
                              )
                            ]
                        , A.attribute "aria-label" description
                        , interaction
                        ]
                        []
                    , if reviewNeeded then
                        Html.span [ A.class "text-base font-bold leading-none text-amber-700", A.attribute "aria-hidden" "true" ] [ Html.text "!" ]

                      else
                        Html.text ""
                    ]
                , Html.span
                    [ A.classList [ ( "min-w-0 break-words", True ), ( "text-slate-400 line-through", complete ) ]
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


itemActions : SaveState -> Domain.Pending -> Html Msg
itemActions state item =
    case item.fields.content of
        Separator _ ->
            reviewButton state (DeleteSeparator (Domain.ref item)) ("Borrar definitivamente " ++ itemLabel item) "✕"

        PendingTask _ _ ->
            editButton state item


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
        , A.attribute "aria-label"
            ("Arrastrar "
                ++ itemLabel item
                ++ (case item.fields.content of
                        PendingTask _ _ ->
                            ". También puedes cambiar su posición desde Editar."

                        Separator _ ->
                            ""
                   )
            )
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
        , itemActions state item
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
            if isReadOnly draft then
                Saving

            else
                state

        wasDone =
            Domain.pendingCompletion (draftFields draft).content == Just Done
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
        , field "pending-owner"
            "Responsable"
            (Html.select [ A.id "pending-owner", A.class inputClass, A.value (Domain.ownerLabel draft.owner), A.disabled (fieldState == Saving || wasDone), E.onInput OwnerChanged ]
                (List.map (\owner -> Html.option [ A.value (Domain.ownerLabel owner) ] [ Html.text (Domain.ownerLabel owner) ]) [ Rolando, Dot, Others ])
            )
        , completionField fieldState draft
        , if draft.target == Adding then
            Html.text ""

          else
            editorOptions fieldState draft
        , if wasDone && not (isReadOnly draft) then
            Html.p [ A.class "text-sm text-slate-600" ]
                [ Html.text "Para cambiar el responsable o el tipo, primero guarda el estado Pendiente." ]

          else
            Html.text ""
        , if isReadOnly draft then
            Html.p [ A.class "text-base text-slate-600" ]
                [ Html.text "Restaura para modificar este pendiente. Borrar definitivamente lo elimina del tablero y de sus próximos respaldos." ]

          else
            Html.text ""
        , Html.div [ A.class "flex flex-wrap gap-2" ]
            (editorButtons state draft
                ++ [ button state Cancel "Cerrar" ]
            )
        , if draft.target == Adding then
            -- Keep the quick entry path in natural DOM order: text, owner, save.
            Html.details [ A.class "space-y-4" ]
                [ Html.summary [ A.class "cursor-pointer text-sm font-medium" ] [ Html.text "Más opciones: tipo y posición" ]
                , editorOptions fieldState draft
                ]

          else
            Html.text ""
        , if error /= Nothing && (isReadOnly draft || submission == StatusChange) then
            Html.div [ A.class "space-y-2 text-sm text-slate-600" ]
                [ Html.p [] [ Html.text "Actualiza la versión antes de volver a descartar, restaurar o borrar. Este botón no modifica el registro." ]
                , button state Refresh "Actualizar versión"
                ]

          else if error /= Nothing then
            Html.div [ A.class "space-y-2 text-sm text-slate-600" ]
                [ Html.p [] [ Html.text "Tu texto, estado elegido y movimiento siguen aquí. Revisa la lista antes de aplicarlos sobre la versión actual." ]
                , button state Retry "Aplicar sobre versión actual"
                ]

          else
            Html.text ""
        ]


editorOptions : SaveState -> Draft -> Html Msg
editorOptions state draft =
    Html.div [ A.class "space-y-4" ]
        [ field "pending-kind"
            "Tipo"
            (Html.select
                [ A.id "pending-kind"
                , A.class inputClass
                , A.value (contentKind draft.content)
                , A.disabled (state == Saving || Domain.pendingCompletion (draftFields draft).content == Just Done)
                , E.onInput KindChanged
                ]
                [ Html.option [ A.value "task" ] [ Html.text "Pendiente" ]
                , Html.option [ A.value "blank" ] [ Html.text "Separador vacío" ]
                , Html.option [ A.value "separator" ] [ Html.text "Separador con texto" ]
                ]
            )
        , if isReadOnly draft || draftCompletion draft == Done then
            Html.text ""

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


completionField : SaveState -> Draft -> Html Msg
completionField state draft =
    case ( draft.target, draft.content ) of
        ( Saved _ _, TaskDraft _ status ) ->
            let
                needsApproval =
                    case status of
                        AwaitingApproval _ ->
                            True

                        _ ->
                            False
            in
            Html.div [ A.class "space-y-1" ]
                [ field "pending-completion"
                    "Estado"
                    (Html.select
                        [ A.id "pending-completion"
                        , A.class inputClass
                        , A.value
                            (if draftCompletion draft == Done then
                                "completed"

                             else
                                "pending"
                            )
                        , A.disabled (state == Saving)
                        , E.onInput CompletionChanged
                        ]
                        [ Html.option [ A.value "pending" ] [ Html.text "Pendiente" ]
                        , Html.option [ A.value "completed", A.disabled needsApproval ] [ Html.text "Completado" ]
                        ]
                    )
                , if needsApproval then
                    Html.p [ A.class "text-sm text-slate-600" ] [ Html.text "Primero debe aprobarse la tarea para marcarla como completada." ]

                  else
                    Html.text ""
                ]

        _ ->
            Html.text ""


field : String -> String -> Html Msg -> Html Msg
field controlId title control =
    Html.div [ A.class "min-w-0 space-y-1" ]
        [ Html.label [ A.for controlId, A.class "block text-base font-medium" ] [ Html.text title ]
        , control
        ]



-- Compact controls keep their full action names for keyboard and screen-reader users.


reviewButton : SaveState -> Msg -> String -> String -> Html Msg
reviewButton state msg title symbol =
    Html.button
        [ A.type_ "button"
        , E.onClick msg
        , A.disabled (state == Saving)
        , A.title title
        , A.attribute "aria-label" title
        , A.class "inline-flex h-7 w-7 items-center justify-center rounded border border-slate-300 bg-white text-base font-semibold text-slate-700 hover:bg-slate-50 disabled:opacity-50"
        ]
        [ Html.span [ A.attribute "aria-hidden" "true" ] [ Html.text symbol ] ]


editButton : SaveState -> Domain.Pending -> Html Msg
editButton state item =
    Html.button
        [ A.type_ "button"
        , E.onClick (Edit item.id)
        , A.disabled (state == Saving)
        , A.class "shrink-0 rounded border border-slate-300 bg-white px-1.5 py-0.5 text-[0.75rem] leading-5 font-medium text-slate-700 hover:bg-slate-50 disabled:opacity-50"
        , A.attribute "aria-label" ("Editar " ++ itemLabel item)
        ]
        [ Html.text "Editar" ]


button : SaveState -> Msg -> String -> Html Msg
button state msg title =
    Html.button
        ([ A.type_ "button", E.onClick msg, A.disabled (state == Saving), A.class "rounded border border-slate-300 px-3 py-2 text-base font-medium hover:bg-slate-50 disabled:opacity-50" ]
            ++ (case msg of
                    AddTask ->
                        [ A.id "pending-add", A.title "Añadir pendiente (Ctrl+A)", A.attribute "aria-keyshortcuts" "Control+A" ]

                    _ ->
                        []
               )
        )
        (case msg of
            AddTask ->
                [ Html.u [] [ Html.text "A" ], Html.text "ñadir pendiente" ]

            _ ->
                [ Html.text title ]
        )


inputClass : String
inputClass =
    "block w-full min-w-0 rounded border border-slate-300 bg-white px-3 py-2 text-base text-slate-900 disabled:opacity-60"
