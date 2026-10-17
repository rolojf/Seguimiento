module Processes exposing (Model, Msg, init, isLocal, isModal, settle, subscriptions, update, view)

import Browser.Dom
import Browser.Events
import Dict
import Domain exposing (Applicability(..), Board, Change(..), Checkbox(..), Closure(..), Completion(..), Process, ProcessFields, ProcessKind(..), ProcessStatus(..), Ref, Review(..), SaveState(..), Task, TaskChange(..), User(..))
import Html exposing (Html, button, caption, details, div, form, h2, h3, input, label, option, p, progress, section, select, span, strong, summary, table, tbody, td, text, textarea, th, thead, tr)
import Html.Attributes as Attr exposing (attribute, checked, class, disabled, for, id, maxlength, placeholder, required, rows, scope, selected, type_, value)
import Html.Events exposing (onBlur, onCheck, onClick, onFocus, onInput, onMouseEnter, onMouseLeave, onSubmit, preventDefaultOn)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Url


type Model
    = Model
        { editor : Editor
        , error : String
        , tooltip : Tooltip
        , filter : ProcessStatus
        , focus : Maybe String
        }



-- Each editor captures the displayed revision; a conflict keeps its draft intact.


type Editor
    = Browsing
    | Creating Draft
    | TakingNotes NotesAccess NotesDraft
    | Completing Ref String
    | Purging Ref String


type NotesAccess
    = EditableNotes
    | ReadOnlyNotes


type alias NotesDraft =
    { ref : Ref
    , task : String
    , title : String
    , notes : String
    , opener : String
    }


type Tooltip
    = Hidden
    | Hovered TipRef
    | Focused TipRef


type alias TipRef =
    { process : String, task : String }


type alias Draft =
    { client : String
    , kind : Maybe ProcessKind
    , startDate : String
    , context : String
    }


type Field
    = Client
    | Kind
    | StartDate
    | Context


type Msg
    = AddProcess
    | Cancel
    | Edit Field String
    | Create
    | SelectFilter ProcessStatus
    | ChangeStatus Ref String TaskChange
    | OpenNotes NotesAccess NotesDraft
    | EditNotes String
    | SaveNotes
    | AskComplete Ref String
    | AskPurge Ref String
    | ConfirmAction
    | CloseDialog
    | FocusAt String
    | FocusNext
    | FocusedElement
    | HoverTip TipRef
    | FocusTip TipRef
    | LeaveTip TipRef
    | BlurTip TipRef
    | CloseTip


init : Model
init =
    Model { editor = Browsing, error = "", tooltip = Hidden, filter = ActiveProcess, focus = Nothing }


subscriptions : Model -> Sub Msg
subscriptions ((Model state) as model) =
    Sub.batch
        [ if isModal model then
            Browser.Events.onKeyDown (escapeKey CloseDialog)

          else if state.tooltip /= Hidden then
            Browser.Events.onKeyDown (escapeKey CloseTip)

          else
            Sub.none
        , case state.focus of
            Just _ ->
                Browser.Events.onAnimationFrame (\_ -> FocusNext)

            Nothing ->
                Sub.none
        ]


update : Msg -> Board -> Model -> ( Model, Cmd Msg, Maybe Change )
update msg _ ((Model state) as model) =
    case ( msg, state.editor ) of
        ( AddProcess, Browsing ) ->
            ( Model { state | editor = Creating { client = "", kind = Nothing, startDate = "", context = "" }, error = "", filter = ActiveProcess, focus = Nothing }, focusOn "process-client", Nothing )

        ( Cancel, Creating _ ) ->
            ( Model { state | editor = Browsing, error = "", focus = Nothing }, focusOn "process-add", Nothing )

        ( Edit field inputValue, Creating draft ) ->
            ( Model { state | editor = Creating (editDraft field inputValue draft), error = "" }, Cmd.none, Nothing )

        ( Create, Creating draft ) ->
            case processFields draft of
                Ok fields ->
                    ( Model { state | error = "" }, Cmd.none, Just (CreateProcess fields) )

                Err message ->
                    ( Model { state | error = message }, Cmd.none, Nothing )

        ( SelectFilter filter, Browsing ) ->
            ( Model { state | filter = filter, error = "", tooltip = Hidden, focus = Nothing }, Cmd.none, Nothing )

        ( ChangeStatus processRef taskId change, Browsing ) ->
            ( Model { state | error = "" }, Cmd.none, Just (ChangeTask processRef taskId change) )

        ( OpenNotes access draft, Browsing ) ->
            ( Model { state | editor = TakingNotes access draft, error = "", tooltip = Hidden, focus = Nothing }, focusOn "process-note-text", Nothing )

        ( EditNotes notes, TakingNotes EditableNotes draft ) ->
            ( Model { state | editor = TakingNotes EditableNotes { draft | notes = notes }, error = "" }, Cmd.none, Nothing )

        ( SaveNotes, TakingNotes EditableNotes draft ) ->
            ( Model { state | error = "" }, focusOn "process-dialog", Just (SaveTaskNotes draft.ref draft.task draft.notes) )

        ( AskComplete processRef client, Browsing ) ->
            ( Model { state | editor = Completing processRef client, error = "", tooltip = Hidden, focus = Nothing }, focusOn "process-dialog-cancel", Nothing )

        ( AskPurge processRef client, Browsing ) ->
            ( Model { state | editor = Purging processRef client, error = "", tooltip = Hidden, focus = Nothing }, focusOn "process-dialog-cancel", Nothing )

        ( ConfirmAction, Completing processRef _ ) ->
            ( Model { state | error = "" }, focusOn "process-dialog", Just (CompleteProcess processRef) )

        ( ConfirmAction, Purging processRef _ ) ->
            ( Model { state | error = "" }, focusOn "process-dialog", Just (PurgeProcess processRef) )

        ( CloseDialog, _ ) ->
            if isModal model then
                ( Model { state | editor = Browsing, error = "", focus = Nothing }, focusOn (returnFocus state.editor state.filter), Nothing )

            else
                ( model, Cmd.none, Nothing )

        ( FocusAt target, _ ) ->
            ( Model { state | focus = Nothing }, focusOn target, Nothing )

        ( FocusNext, _ ) ->
            ( Model { state | focus = Nothing }, Maybe.map focusOn state.focus |> Maybe.withDefault Cmd.none, Nothing )

        ( HoverTip tip, Browsing ) ->
            ( Model
                { state
                    | tooltip =
                        if state.tooltip == Focused tip then
                            state.tooltip

                        else
                            Hovered tip
                }
            , Cmd.none
            , Nothing
            )

        ( FocusTip tip, Browsing ) ->
            ( Model { state | tooltip = Focused tip }, Cmd.none, Nothing )

        ( LeaveTip tip, _ ) ->
            ( Model
                { state
                    | tooltip =
                        if state.tooltip == Hovered tip then
                            Hidden

                        else
                            state.tooltip
                }
            , Cmd.none
            , Nothing
            )

        ( BlurTip tip, _ ) ->
            ( Model
                { state
                    | tooltip =
                        if tipVisible tip state.tooltip then
                            Hidden

                        else
                            state.tooltip
                }
            , Cmd.none
            , Nothing
            )

        ( CloseTip, _ ) ->
            ( Model { state | tooltip = Hidden }, Cmd.none, Nothing )

        _ ->
            ( model, Cmd.none, Nothing )


settle : Result String () -> Model -> Model
settle result ((Model state) as model) =
    case result of
        Ok () ->
            let
                filter =
                    case state.editor of
                        Completing _ _ ->
                            CompletedProcess

                        _ ->
                            state.filter
            in
            Model
                { state
                    | editor = Browsing
                    , error = ""
                    , filter = filter
                    , focus =
                        case state.editor of
                            Browsing ->
                                Nothing

                            Completing _ _ ->
                                Just (filterId CompletedProcess)

                            Purging _ _ ->
                                Just (filterId CompletedProcess)

                            _ ->
                                Just (returnFocus state.editor filter)
                }

        Err error ->
            Model
                { state
                    | error = error
                    , focus =
                        if isModal model then
                            Just "process-dialog"

                        else
                            Nothing
                }


focusOn : String -> Cmd Msg
focusOn target =
    Browser.Dom.focus target
        |> Task.onError (\_ -> Browser.Dom.focus "process-add")
        |> Task.attempt (\_ -> FocusedElement)


returnFocus : Editor -> ProcessStatus -> String
returnFocus editor filter =
    case editor of
        TakingNotes _ draft ->
            draft.opener

        Completing processRef _ ->
            actionId processRef.id

        Purging processRef _ ->
            actionId processRef.id

        Creating _ ->
            "process-add"

        Browsing ->
            filterId filter


isModal : Model -> Bool
isModal (Model state) =
    case state.editor of
        TakingNotes _ _ ->
            True

        Completing _ _ ->
            True

        Purging _ _ ->
            True

        _ ->
            False


editDraft : Field -> String -> Draft -> Draft
editDraft field newValue draft =
    case field of
        Client ->
            { draft | client = newValue }

        Kind ->
            { draft
                | kind =
                    case newValue of
                        "nueva_oti" ->
                            Just NewOti

                        "entregas" ->
                            Just Deliveries

                        _ ->
                            Nothing
            }

        StartDate ->
            { draft | startDate = newValue }

        Context ->
            { draft | context = newValue }


processFields : Draft -> Result String ProcessFields
processFields draft =
    if String.isEmpty (String.trim draft.client) then
        Err "Escribe el nombre del cliente."

    else if String.isEmpty draft.startDate then
        Err "Indica la fecha real de inicio."

    else
        case draft.kind of
            Nothing ->
                Err "Selecciona el tipo de proceso."

            Just kind ->
                Ok
                    { client = String.trim draft.client
                    , kind = kind
                    , startDate = draft.startDate
                    , context = String.trim draft.context
                    }


view : SaveState -> User -> Board -> Model -> Html Msg
view saving user board ((Model state) as model) =
    let
        processes =
            Dict.values board.processes
                |> List.filter (\process -> process.status == state.filter)
                |> List.sortBy (\process -> ( process.fields.startDate, process.id ))
                |> List.reverse

        locked =
            saving == Saving || state.editor /= Browsing
    in
    section [ id "procesos", attribute "aria-labelledby" "processes-title" ]
        [ div [ class "space-y-5", Attr.property "inert" (Encode.bool (isModal model)) ]
            [ div [ class "flex flex-wrap items-center justify-between gap-3" ]
                [ div []
                    [ h2 [ id "processes-title", class "text-2xl font-semibold text-slate-900" ] [ text "Procesos" ]
                    , p [ class "mt-1 text-sm text-slate-600" ] [ text "Nueva OTI y entregas, paso a paso." ]
                    ]
                , button [ id "process-add", type_ "button", class primaryButton, disabled locked, Attr.title "Añadir proceso (Ctrl+R)", attribute "aria-keyshortcuts" "Control+R", onClick AddProcess ]
                    [ text "+ Agregar p", Html.u [] [ text "r" ], text "oceso" ]
                ]
            , div [ class "flex flex-wrap gap-2", attribute "aria-label" "Filtrar procesos" ]
                [ filterButton saving model ActiveProcess "Activos"
                , filterButton saving model CompletedProcess "Completados"
                ]
            , totalsView processes
            , errorView state.error
            , case state.editor of
                Creating draft ->
                    createForm saving draft

                _ ->
                    text ""
            , if List.isEmpty processes then
                p [ class "rounded-xl border border-dashed border-slate-300 p-8 text-center text-slate-600" ]
                    [ text
                        (if state.filter == ActiveProcess then
                            "No hay procesos activos. Agrega uno con su cliente, tipo y fecha de inicio."

                         else
                            "No hay procesos completados."
                        )
                    ]

              else
                Keyed.node "div" [ class "grid grid-cols-1 items-start gap-4 xl:grid-cols-2" ] (List.map (\process -> ( process.id, processView saving user model process )) processes)
            , p [ class "text-sm text-slate-500" ]
                [ text
                    (if state.filter == ActiveProcess then
                        "N/A excluye el paso del avance y conserva su estado. «!» indica un cambio de otro usuario: el primer clic lo reconoce y el siguiente cambia la casilla."

                     else
                        "Los procesos completados se conservan para consulta."
                    )
                ]
            ]
        , dialogView saving model
        ]


filterButton : SaveState -> Model -> ProcessStatus -> String -> Html Msg
filterButton saving (Model state) filter title =
    button
        [ id (filterId filter)
        , type_ "button"
        , class
            (if state.filter == filter then
                primaryButton

             else
                secondaryButton
            )
        , disabled (saving == Saving || state.editor /= Browsing)
        , attribute "aria-pressed"
            (if state.filter == filter then
                "true"

             else
                "false"
            )
        , onClick (SelectFilter filter)
        ]
        [ text title ]


filterId : ProcessStatus -> String
filterId filter =
    case filter of
        ActiveProcess ->
            "process-filter-active"

        CompletedProcess ->
            "process-filter-completed"


errorView : String -> Html Msg
errorView error =
    if String.isEmpty error then
        text ""

    else
        p [ class "rounded-lg bg-red-50 p-3 text-base text-red-800", attribute "role" "alert" ] [ text error ]


totalsView : List Process -> Html Msg
totalsView processes =
    let
        counts =
            List.map Domain.progress processes

        closed =
            List.filter (\process -> process.closure /= Open) processes |> List.length
    in
    div [ class "rounded-xl border border-slate-200 bg-white p-4" ]
        [ div [ class "grid grid-cols-3 gap-3 text-sm text-slate-600" ]
            [ totalItem (String.fromInt (List.length processes)) "procesos"
            , totalItem (String.fromInt (List.sum (List.map .done counts)) ++ "/" ++ String.fromInt (List.sum (List.map .applicable counts))) "pasos confirmados"
            , totalItem (String.fromInt closed) "cierres personales de Rolo"
            ]
        , p [ class "mt-3 text-sm text-slate-500" ]
            [ text (String.fromInt (List.sum (List.map .excluded counts)) ++ " N/A fuera del avance. Un cierre personal no confirma los pasos.") ]
        ]


totalItem : String -> String -> Html Msg
totalItem amount description =
    div []
        [ strong [ class "block text-xl font-semibold text-slate-900" ] [ text amount ]
        , span [] [ text description ]
        ]


createForm : SaveState -> Draft -> Html Msg
createForm saving draft =
    let
        busy =
            saving == Saving
    in
    form [ class "space-y-4 rounded-xl border border-indigo-200 bg-indigo-50 p-5", onSubmit Create ]
        [ h3 [ class "font-semibold text-slate-900" ] [ text "Agregar proceso" ]
        , p [ class "text-sm text-slate-600" ] [ text "Se incluirán todos los pasos de la plantilla elegida." ]
        , div [ class "grid gap-4 md:grid-cols-3" ]
            [ formField "process-client"
                "Cliente"
                (input [ id "process-client", class fieldClass, value draft.client, onInput (Edit Client), disabled busy, maxlength 90, required True, placeholder "Nombre del cliente" ] [])
            , formField "process-kind"
                "Tipo"
                (select [ id "process-kind", class fieldClass, disabled busy, required True, onInput (Edit Kind) ]
                    [ option [ value "", selected (draft.kind == Nothing) ] [ text "Selecciona un tipo" ]
                    , kindOption draft.kind NewOti "nueva_oti"
                    , kindOption draft.kind Deliveries "entregas"
                    ]
                )
            , formField "process-start"
                "Fecha real de inicio"
                (input [ id "process-start", class fieldClass, type_ "date", value draft.startDate, onInput (Edit StartDate), disabled busy, required True ] [])
            ]
        , formField "process-context"
            "Contexto (opcional)"
            (textarea [ id "process-context", class fieldClass, rows 2, maxlength 240, value draft.context, disabled busy, onInput (Edit Context), placeholder "OTI, producto o referencia útil" ] [])
        , div [ class "flex flex-wrap gap-2" ]
            [ button [ type_ "submit", class primaryButton, disabled busy ]
                [ text
                    (if busy then
                        "Creando…"

                     else
                        "Crear proceso"
                    )
                ]
            , button [ type_ "button", class secondaryButton, disabled busy, onClick Cancel ] [ text "Cancelar" ]
            ]
        ]


kindOption : Maybe ProcessKind -> ProcessKind -> String -> Html Msg
kindOption current kind code =
    option [ value code, selected (current == Just kind) ]
        [ text (Domain.processLabel kind ++ " · " ++ String.fromInt (List.length (Domain.tasks kind)) ++ " pasos") ]


formField : String -> String -> Html Msg -> Html Msg
formField controlId title control =
    div [ class "space-y-1" ]
        [ label [ for controlId, class "block text-base font-medium text-slate-700" ] [ text title ]
        , control
        ]


dialogView : SaveState -> Model -> Html Msg
dialogView saving ((Model state) as model) =
    if isModal model then
        div [ class "fixed inset-0 z-50 flex items-start justify-center overflow-y-auto bg-slate-900/60 p-3 sm:p-6" ]
            [ div
                [ id "process-dialog"
                , attribute "role" "dialog"
                , attribute "aria-modal" "true"
                , attribute "aria-labelledby" "process-dialog-title"
                , Attr.tabindex -1
                , class "my-auto w-full max-w-xl space-y-4 rounded-xl bg-white p-4 text-slate-900 shadow-xl sm:p-6"
                , preventDefaultOn "keydown" (dialogKeys saving state.editor)
                ]
                [ h3 [ id "process-dialog-title", class "text-xl font-semibold" ] [ text (dialogTitle state.editor) ]
                , errorView state.error
                , case state.editor of
                    TakingNotes access draft ->
                        notesForm saving access draft

                    Completing _ client ->
                        confirmView saving
                            ("¿Completar el proceso de " ++ client ++ "? Sus pasos y notas pasarán a Completados para consulta.")
                            "Completar proceso"

                    Purging _ client ->
                        confirmView saving
                            ("¿Borrar definitivamente el proceso de " ++ client ++ "? Se eliminarán también sus pasos y notas. Esta acción no se puede deshacer.")
                            "Borrar definitivamente"

                    _ ->
                        text ""
                , if saving == Saving then
                    p [ class "text-sm text-slate-600", attribute "role" "status" ] [ text "Guardando…" ]

                  else
                    text ""
                ]
            ]

    else
        text ""


dialogTitle : Editor -> String
dialogTitle editor =
    case editor of
        TakingNotes _ _ ->
            "Notas del paso"

        Completing _ _ ->
            "Completar proceso"

        Purging _ _ ->
            "Borrar proceso"

        _ ->
            "Proceso"


notesForm : SaveState -> NotesAccess -> NotesDraft -> Html Msg
notesForm saving access draft =
    form [ class "space-y-4", onSubmit SaveNotes ]
        [ p [ class "break-words text-base font-medium", Attr.style "overflow-wrap" "anywhere" ] [ text draft.title ]
        , formField "process-note-text"
            (if access == ReadOnlyNotes then
                "Notas (solo lectura)"

             else
                "Notas"
            )
            (textarea
                [ id "process-note-text"
                , class fieldClass
                , rows 7
                , maxlength 10000
                , value draft.notes
                , Attr.readonly (access == ReadOnlyNotes || saving == Saving)
                , onInput EditNotes
                , placeholder
                    (if access == ReadOnlyNotes then
                        "Este paso no tiene notas."

                     else
                        "Escribe una nota para este paso."
                    )
                ]
                []
            )
        , div [ class "flex flex-wrap gap-2" ]
            [ if access == EditableNotes then
                button [ id "process-note-save", type_ "submit", class primaryButton, disabled (saving == Saving) ] [ text "Guardar" ]

              else
                text ""
            , button [ id "process-dialog-cancel", type_ "button", class secondaryButton, disabled (saving == Saving), onClick CloseDialog ]
                [ text
                    (if access == ReadOnlyNotes then
                        "Cerrar"

                     else
                        "Cancelar"
                    )
                ]
            ]
        ]


confirmView : SaveState -> String -> String -> Html Msg
confirmView saving explanation action =
    div [ class "space-y-4" ]
        [ p [ class "break-words text-base", Attr.style "overflow-wrap" "anywhere" ] [ text explanation ]
        , div [ class "flex flex-wrap gap-2" ]
            [ button [ id "process-dialog-cancel", type_ "button", class secondaryButton, disabled (saving == Saving), onClick CloseDialog ] [ text "Cancelar" ]
            , button [ id "process-dialog-confirm", type_ "button", class primaryButton, disabled (saving == Saving), onClick ConfirmAction ] [ text action ]
            ]
        ]


type Direction
    = Forward
    | Backward


dialogKeys : SaveState -> Editor -> Decode.Decoder ( Msg, Bool )
dialogKeys saving editor =
    Decode.map3
        (\key shift target -> ( key, shift, target ))
        (Decode.field "key" Decode.string)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.at [ "target", "id" ] Decode.string)
        |> Decode.andThen
            (\( key, shift, target ) ->
                if key == "Tab" then
                    Decode.succeed
                        ( FocusAt
                            (nextFocus (focusTargets saving editor)
                                target
                                (if shift then
                                    Backward

                                 else
                                    Forward
                                )
                            )
                        , True
                        )

                else
                    Decode.fail "Other key"
            )


focusTargets : SaveState -> Editor -> List String
focusTargets saving editor =
    -- During a save all actions are disabled; keep Tab on the dialog itself.
    if saving == Saving then
        [ "process-dialog" ]

    else
        case editor of
            TakingNotes EditableNotes _ ->
                [ "process-note-text", "process-note-save", "process-dialog-cancel" ]

            TakingNotes ReadOnlyNotes _ ->
                [ "process-note-text", "process-dialog-cancel" ]

            _ ->
                [ "process-dialog-cancel", "process-dialog-confirm" ]


nextFocus : List String -> String -> Direction -> String
nextFocus targets current direction =
    let
        position =
            List.indexedMap Tuple.pair targets
                |> List.filter (\( _, target ) -> target == current)
                |> List.head
                |> Maybe.map Tuple.first

        index =
            case position of
                Just found ->
                    modBy (List.length targets)
                        (found
                            + (if direction == Backward then
                                -1

                               else
                                1
                              )
                        )

                Nothing ->
                    if direction == Backward then
                        List.length targets - 1

                    else
                        0
    in
    List.drop index targets |> List.head |> Maybe.withDefault "process-dialog"


processView : SaveState -> User -> Model -> Process -> Html Msg
processView saving user model process =
    details [ class "min-w-0 rounded-xl border border-slate-200 bg-white shadow-sm" ]
        [ summary [ class "cursor-pointer break-words rounded-xl p-3 focus-visible:outline focus-visible:outline-2 focus-visible:outline-indigo-600" ]
            [ span [ class "font-semibold text-slate-900" ] [ text process.fields.client ]
            , span [ class "ml-3 text-sm text-slate-500" ] [ text (Domain.processLabel process.fields.kind) ]
            , span [ class "mt-2 block text-sm text-slate-600" ]
                [ text ("Inicio: " ++ dateLabel process.fields.startDate ++ " · " ++ progressLabel process) ]
            , if process.status == CompletedProcess then
                span [ class "mt-2 inline-block rounded bg-emerald-50 px-2 py-1 text-sm text-emerald-800" ] [ text "Completado · solo lectura" ]

              else
                text ""
            , case process.closure of
                PersonallyClosed _ ->
                    span [ class "mt-2 inline-block rounded bg-amber-100 px-2 py-1 text-sm text-amber-900" ] [ text "Cierre personal de Rolo" ]

                Open ->
                    text ""
            ]
        , div [ class "space-y-3 border-t border-slate-100 p-2 sm:p-3" ]
            [ optionalText process.fields.context
            , closureView process
            , progressView process
            , nextStep process
            , legacyView process.legacySummary
            , processAction saving user model process
            , div []
                [ table [ class "w-full table-fixed border-collapse text-base" ]
                    [ caption [ class "sr-only" ]
                        [ text (Domain.processLabel process.fields.kind ++ " de " ++ process.fields.client ++ ". Cada paso tiene un solo responsable.") ]
                    , thead []
                        [ tr []
                            [ th [ scope "col", class "w-8 border-b border-slate-200 py-2 text-center text-sm font-medium text-slate-600" ] [ text "N/A" ]
                            , th [ scope "col", class "border-b border-slate-200 px-1 py-2 text-left text-sm font-medium text-slate-600", Attr.style "overflow-wrap" "anywhere" ] [ text "Descripción" ]
                            , ownerHeading "dot"
                            , ownerHeading "Rolando"
                            , ownerHeading "Otros"
                            , th [ scope "col", class "w-10 border-b border-slate-200 py-2 text-center text-sm font-medium text-slate-600" ] [ text "Notas" ]
                            ]
                        ]
                    , tbody [] (List.map (taskRow saving user model process) (Domain.tasks process.fields.kind))
                    ]
                ]
            ]
        ]


processAction : SaveState -> User -> Model -> Process -> Html Msg
processAction saving user (Model state) process =
    if user == UserOne then
        button
            [ id (actionId process.id)
            , type_ "button"
            , class secondaryButton
            , disabled (saving == Saving || state.editor /= Browsing)
            , onClick
                (case process.status of
                    ActiveProcess ->
                        AskComplete (Domain.ref process) process.fields.client

                    CompletedProcess ->
                        AskPurge (Domain.ref process) process.fields.client
                )
            ]
            [ text
                (case process.status of
                    ActiveProcess ->
                        "Completar proceso"

                    CompletedProcess ->
                        "Borrar definitivamente"
                )
            ]

    else
        text ""


actionId : String -> String
actionId processId =
    "process-action-" ++ Url.percentEncode processId


progressLabel : Process -> String
progressLabel process =
    let
        counts =
            Domain.progress process
    in
    (if counts.applicable == 0 then
        "Sin pasos aplicables"

     else
        String.fromInt counts.done ++ " de " ++ String.fromInt counts.applicable ++ " confirmados"
    )
        ++ " · "
        ++ String.fromInt counts.excluded
        ++ " N/A"


progressView : Process -> Html Msg
progressView process =
    let
        counts =
            Domain.progress process
    in
    div [ class "space-y-1" ]
        [ if counts.applicable > 0 then
            progress
                [ class "block h-2 w-full accent-indigo-600"
                , Attr.max (String.fromInt counts.applicable)
                , value (String.fromInt counts.done)
                , attribute "aria-label" ("Pasos confirmados de " ++ process.fields.client)
                , attribute "aria-valuetext" (progressLabel process)
                ]
                []

          else
            text ""
        , p [ class "text-sm text-slate-600" ] [ text (progressLabel process) ]
        ]


closureView : Process -> Html Msg
closureView process =
    case process.closure of
        Open ->
            text ""

        PersonallyClosed reason ->
            div [ class "space-y-1 rounded-lg bg-amber-50 p-3 text-sm text-amber-900", Attr.style "overflow-wrap" "anywhere" ]
                [ strong [] [ text "Cierre personal de Rolo" ]
                , p [] [ text reason ]
                , p [] [ text "El cierre personal se conserva. Los pasos pendientes siguen por confirmar y el avance se calcula por separado." ]
                ]


nextStep : Process -> Html Msg
nextStep process =
    if process.closure /= Open || process.status == CompletedProcess then
        text ""

    else
        let
            pending =
                Domain.tasks process.fields.kind
                    |> List.filter (\task -> (Domain.taskState task.id process).applicability == Applicable Todo)

            description =
                case List.head pending of
                    Just task ->
                        "Sigue: " ++ task.label ++ " · " ++ ownerLabel task.owner

                    Nothing ->
                        if (Domain.progress process).applicable == 0 then
                            "Todos los pasos se marcaron como no aplicables."

                        else
                            "Todos los pasos aplicables están confirmados."
        in
        p [ class "rounded-lg bg-slate-50 p-3 text-base text-slate-700" ] [ text description ]


legacyView : String -> Html Msg
legacyView legacy =
    if String.isEmpty legacy then
        text ""

    else
        details [ class "rounded-lg border border-slate-200 p-3 text-sm text-slate-600" ]
            [ summary [ class "cursor-pointer font-medium" ] [ text "Ver registro anterior conservado" ]
            , p [ class "mt-2 whitespace-pre-wrap", Attr.style "overflow-wrap" "anywhere" ] [ text legacy ]
            , p [ class "mt-2 text-sm" ] [ text "Este registro no confirma automáticamente los pasos de la plantilla." ]
            ]


taskRow : SaveState -> User -> Model -> Process -> Task -> Html Msg
taskRow saving user (Model state) process task =
    let
        current =
            Domain.taskState task.id process
    in
    tr []
        ([ td [ class "border-b border-slate-100 py-1 align-top" ]
            [ fieldBox saving user state.editor process task ApplicabilityBox ]
         , td [ class "border-b border-slate-100 px-1 py-1 align-top" ]
            [ taskName state.tooltip process task
            , if process.closure /= Open && current.applicability == Applicable Todo then
                p [ class "text-sm text-amber-800", Attr.style "overflow-wrap" "anywhere" ] [ text "Por confirmar" ]

              else
                text ""
            , case current.evidence of
                Just evidence ->
                    p [ class "mt-1 whitespace-pre-wrap break-words text-sm text-emerald-800", Attr.style "overflow-wrap" "anywhere" ]
                        [ strong [] [ text "Evidencia: " ], text evidence ]

                Nothing ->
                    text ""
            ]
         ]
            ++ List.map (ownerCell saving user state.editor process task) [ "dot", "Rolando", "Otros" ]
            ++ [ notesCell saving state.editor process task ]
        )


notesCell : SaveState -> Editor -> Process -> Task -> Html Msg
notesCell saving editor process task =
    let
        notes =
            (Domain.taskState task.id process).notes

        hasNotes =
            not (String.isEmpty notes)

        opener =
            "process-notes-" ++ Url.percentEncode process.id ++ ":" ++ Url.percentEncode task.id

        access =
            if process.status == ActiveProcess then
                EditableNotes

            else
                ReadOnlyNotes

        description =
            (if hasNotes then
                "Ver notas: "

             else if access == EditableNotes then
                "Agregar notas: "

             else
                "Sin notas: "
            )
                ++ task.label
    in
    td [ class "border-b border-slate-100 py-1 text-center align-top" ]
        [ button
            [ id opener
            , type_ "button"
            , class
                ("inline-flex h-8 w-8 items-center justify-center rounded border text-base font-semibold disabled:opacity-50 "
                    ++ (if hasNotes then
                            "border-indigo-300 bg-indigo-50 text-indigo-700"

                        else
                            "border-slate-200 bg-white text-slate-500"
                       )
                )
            , disabled (saving == Saving || editor /= Browsing)
            , Attr.title description
            , attribute "aria-label" description
            , attribute "aria-haspopup" "dialog"
            , onClick
                (OpenNotes access
                    { ref = Domain.ref process
                    , task = task.id
                    , title = process.fields.client ++ " · " ++ task.label
                    , notes = notes
                    , opener = opener
                    }
                )
            ]
            [ span [ attribute "aria-hidden" "true" ]
                [ text
                    (if hasNotes then
                        "≡"

                     else
                        "+"
                    )
                ]
            ]
        ]


ownerLabel : String -> String
ownerLabel owner =
    case owner of
        "Rolando" ->
            "Rolo"

        "dot" ->
            "Dot"

        _ ->
            owner


ownerHeading : String -> Html Msg
ownerHeading owner =
    th
        [ scope "col"
        , class
            ((case owner of
                "Rolando" ->
                    "w-10"

                "dot" ->
                    "w-8"

                _ ->
                    "w-10"
             )
                ++ " border-b border-slate-200 py-2 text-center text-sm font-medium text-slate-600"
            )
        ]
        [ text (ownerLabel owner) ]


ownerCell : SaveState -> User -> Editor -> Process -> Task -> String -> Html Msg
ownerCell saving user editor process task column =
    let
        group =
            if task.owner == "dot" || task.owner == "Rolando" then
                task.owner

            else
                "Otros"
    in
    td [ class "border-b border-slate-100 py-1 text-center align-top" ]
        [ if column /= group then
            text ""

          else
            div []
                [ fieldBox saving user editor process task CompletionBox
                , if column == "Otros" then
                    p [ class "break-words text-sm leading-tight", Attr.style "overflow-wrap" "anywhere" ] [ text task.owner ]

                  else
                    text ""
                ]
        ]


type alias Control =
    { checked : Bool
    , blocked : Bool
    , review : Review
    , toggle : TaskChange
    , label : String
    }


fieldBox : SaveState -> User -> Editor -> Process -> Task -> Checkbox -> Html Msg
fieldBox saving user editor process task box =
    let
        control =
            boxControl process task box

        reviewNeeded =
            control.review == NeedsReview user

        action =
            ChangeStatus (Domain.ref process) task.id

        interaction =
            if reviewNeeded then
                -- Prevent the browser's checkbox toggle on the acknowledgement click.
                preventDefaultOn "click" (Decode.succeed ( action (Acknowledge box), True ))

            else
                onCheck (\_ -> action control.toggle)

        description =
            control.label
                ++ process.fields.client
                ++ ": "
                ++ task.label
                ++ (if reviewNeeded && process.status == CompletedProcess then
                        ". Proceso completado: se conserva el aviso de cambio pendiente de revisar. Solo lectura."

                    else if reviewNeeded then
                        ". Cambio de otro usuario pendiente de revisar. Pulsa para reconocer sin cambiar la casilla."

                    else
                        ""
                   )
    in
    label [ class "inline-flex min-h-8 w-full items-center justify-center gap-0.5" ]
        [ input
            [ type_ "checkbox"
            , class "h-4 w-4 shrink-0 accent-indigo-600 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600"
            , checked control.checked
            , disabled (saving == Saving || editor /= Browsing || process.status == CompletedProcess || (control.blocked && not reviewNeeded))
            , attribute "aria-label" description
            , interaction
            ]
            []
        , if reviewNeeded then
            span
                [ class "text-sm font-bold leading-none text-amber-700"
                , attribute "aria-hidden" "true"
                , Attr.title
                    (if process.status == CompletedProcess then
                        "Cambio pendiente de revisar conservado; proceso de solo lectura."

                     else
                        "Cambio pendiente de revisar."
                    )
                ]
                [ text "!" ]

          else
            text ""
        ]


boxControl : Process -> Task -> Checkbox -> Control
boxControl process task box =
    let
        current =
            Domain.taskState task.id process

        completion =
            case current.applicability of
                Applicable state ->
                    state

                NotApplicable state ->
                    state

        excluded =
            isExcluded current.applicability
    in
    case box of
        ApplicabilityBox ->
            { checked = excluded
            , blocked = not task.allowNotApplicable
            , review = current.applicabilityReview
            , toggle =
                if excluded then
                    RestoreTask

                else
                    ExcludeTask
            , label = "No aplica. "
            }

        CompletionBox ->
            { checked = completion == Done
            , blocked = excluded
            , review = current.completionReview
            , toggle =
                SetCompletion
                    (if completion == Done then
                        Todo

                     else
                        Done
                    )
            , label = "Confirmado por " ++ ownerLabel task.owner ++ ". "
            }


isExcluded : Applicability -> Bool
isExcluded applicability =
    case applicability of
        Applicable _ ->
            False

        NotApplicable _ ->
            True


taskName : Tooltip -> Process -> Task -> Html Msg
taskName tooltip process task =
    let
        tip =
            { process = process.id, task = task.id }

        visible =
            tipVisible tip tooltip

        descriptionId =
            "process-tip-" ++ Url.percentEncode process.id ++ ":" ++ Url.percentEncode task.id

        nameStyle =
            if isExcluded (Domain.taskState task.id process).applicability then
                "text-slate-400 line-through"

            else
                "text-slate-700"
    in
    div
        [ class "relative"
        , onMouseEnter (HoverTip tip)
        , onMouseLeave (LeaveTip tip)
        ]
        [ button
            ([ type_ "button"
             , class ("w-full rounded py-1 text-left text-base font-medium focus-visible:outline focus-visible:outline-2 focus-visible:outline-indigo-600 " ++ nameStyle)
             , Attr.style "overflow-wrap" "anywhere"
             , onFocus (FocusTip tip)
             , onBlur (BlurTip tip)
             , onClick (FocusTip tip)
             ]
                ++ (if visible then
                        [ attribute "aria-describedby" descriptionId ]

                    else
                        []
                   )
            )
            [ text task.label ]
        , if visible then
            -- Owner and notes columns total 9.5rem; keep the tooltip inside the card.
            div [ class "absolute left-0 top-full z-30 pt-1", Attr.style "width" "calc(100% + 9.5rem)" ]
                [ div
                    [ id descriptionId
                    , attribute "role" "tooltip"
                    , class "space-y-2 rounded-lg bg-slate-900 p-3 text-left text-[0.7rem] font-normal leading-normal text-white shadow-lg"
                    , Attr.style "overflow-wrap" "anywhere"
                    ]
                    [ p [] [ text task.detail ]
                    , if String.isEmpty task.condition then
                        text ""

                      else
                        p [ class "text-slate-200" ] [ text task.condition ]
                    ]
                ]

          else
            text ""
        ]


tipVisible : TipRef -> Tooltip -> Bool
tipVisible tip tooltip =
    case tooltip of
        Hidden ->
            False

        Hovered current ->
            current == tip

        Focused current ->
            current == tip


escapeKey : Msg -> Decode.Decoder Msg
escapeKey message =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Escape" then
                    Decode.succeed message

                else
                    Decode.fail "Other key"
            )


isLocal : Msg -> Bool
isLocal msg =
    case msg of
        HoverTip _ ->
            True

        FocusTip _ ->
            True

        LeaveTip _ ->
            True

        BlurTip _ ->
            True

        CloseTip ->
            True

        FocusAt _ ->
            True

        FocusNext ->
            True

        FocusedElement ->
            True

        _ ->
            False


optionalText : String -> Html Msg
optionalText content =
    if String.isEmpty content then
        text ""

    else
        p [ class "mt-2 whitespace-pre-wrap text-sm text-slate-600", Attr.style "overflow-wrap" "anywhere" ] [ text content ]


dateLabel : String -> String
dateLabel raw =
    case String.split "-" raw of
        [ year, month, day ] ->
            day ++ "/" ++ month ++ "/" ++ year

        _ ->
            raw


fieldClass : String
fieldClass =
    "block w-full rounded-lg border border-slate-300 bg-white px-3 py-2 text-base text-slate-900 focus:border-indigo-500 focus:outline-none focus:ring-1 focus:ring-indigo-500 disabled:opacity-50"


primaryButton : String
primaryButton =
    "rounded-lg bg-indigo-600 px-3 py-2 text-base font-medium text-white hover:bg-indigo-700 disabled:cursor-not-allowed disabled:opacity-50"


secondaryButton : String
secondaryButton =
    "rounded-lg border border-slate-300 bg-white px-3 py-2 text-base text-slate-700 hover:bg-slate-50 disabled:cursor-not-allowed disabled:opacity-50"
