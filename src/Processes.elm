module Processes exposing (Model, Msg, init, isLocal, settle, subscriptions, update, view)

import Browser.Events
import Dict
import Domain exposing (Applicability(..), Board, Change(..), Checkbox(..), Closure(..), Completion(..), Process, ProcessFields, ProcessKind(..), Ref, Review(..), SaveState(..), Task, TaskChange(..), User)
import Html exposing (Html, button, caption, details, div, form, h2, h3, input, label, option, p, progress, section, select, span, strong, summary, table, tbody, td, text, textarea, th, thead, tr)
import Html.Attributes as Attr exposing (attribute, checked, class, disabled, for, id, maxlength, placeholder, required, rows, scope, selected, type_, value)
import Html.Events exposing (onBlur, onCheck, onClick, onFocus, onInput, onMouseEnter, onMouseLeave, onSubmit, preventDefaultOn)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Url


type Model
    = Model Editor String Tooltip



-- Tooltip state is independent of saves, so an acknowledgement leaves it open.


type Editor
    = Browsing
    | Creating Draft


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
    | ChangeStatus Ref String TaskChange
    | HoverTip TipRef
    | FocusTip TipRef
    | LeaveTip TipRef
    | BlurTip TipRef
    | CloseTip


init : Model
init =
    Model Browsing "" Hidden


subscriptions : Model -> Sub Msg
subscriptions (Model _ _ tooltip) =
    case tooltip of
        Hidden ->
            Sub.none

        _ ->
            Browser.Events.onKeyDown escapeKey


update : Msg -> Board -> Model -> ( Model, Maybe Change )
update msg _ (Model editor error tooltip) =
    case ( msg, editor ) of
        ( AddProcess, Browsing ) ->
            ( Model (Creating { client = "", kind = Nothing, startDate = "", context = "" }) "" tooltip, Nothing )

        ( Cancel, _ ) ->
            ( Model Browsing "" tooltip, Nothing )

        ( Edit field inputValue, Creating draft ) ->
            ( Model (Creating (editDraft field inputValue draft)) "" tooltip, Nothing )

        ( Create, Creating draft ) ->
            case processFields draft of
                Ok fields ->
                    ( Model editor "" tooltip, Just (CreateProcess fields) )

                Err message ->
                    ( Model editor message tooltip, Nothing )

        ( ChangeStatus processRef taskId change, Browsing ) ->
            ( Model editor "" tooltip, Just (ChangeTask processRef taskId change) )

        ( HoverTip tip, _ ) ->
            ( Model editor
                error
                (if tooltip == Focused tip then
                    tooltip

                 else
                    Hovered tip
                )
            , Nothing
            )

        ( FocusTip tip, _ ) ->
            ( Model editor error (Focused tip), Nothing )

        ( LeaveTip tip, _ ) ->
            ( Model editor
                error
                (if tooltip == Hovered tip then
                    Hidden

                 else
                    tooltip
                )
            , Nothing
            )

        ( BlurTip tip, _ ) ->
            ( Model editor
                error
                (if tipVisible tip tooltip then
                    Hidden

                 else
                    tooltip
                )
            , Nothing
            )

        ( CloseTip, _ ) ->
            ( Model editor error Hidden, Nothing )

        _ ->
            ( Model editor error tooltip, Nothing )


settle : Result String () -> Model -> Model
settle result (Model editor _ tooltip) =
    case result of
        Ok () ->
            Model Browsing "" tooltip

        Err error ->
            Model editor error tooltip


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
view saving user board ((Model editor error _) as model) =
    let
        processes =
            Dict.values board.processes
                |> List.sortBy (\process -> ( process.fields.startDate, process.id ))
                |> List.reverse

        locked =
            saving == Saving || editor /= Browsing
    in
    section [ id "procesos", class "space-y-5", attribute "aria-labelledby" "processes-title" ]
        [ div [ class "flex flex-wrap items-center justify-between gap-3" ]
            [ div []
                [ h2 [ id "processes-title", class "text-2xl font-semibold text-slate-900" ] [ text "Procesos" ]
                , p [ class "mt-1 text-sm text-slate-600" ] [ text "Nueva OTI y entregas, paso a paso." ]
                ]
            , button [ type_ "button", class primaryButton, disabled locked, onClick AddProcess ] [ text "+ Agregar proceso" ]
            ]
        , totalsView processes
        , if String.isEmpty error then
            text ""

          else
            p [ class "rounded-lg bg-red-50 p-3 text-base text-red-800", attribute "role" "alert" ] [ text error ]
        , case editor of
            Creating draft ->
                createForm saving draft

            _ ->
                text ""
        , if List.isEmpty processes then
            p [ class "rounded-xl border border-dashed border-slate-300 p-8 text-center text-slate-600" ]
                [ text "Todavía no hay procesos. Agrega uno con su cliente, tipo y fecha de inicio." ]

          else
            Keyed.node "div" [ class "grid grid-cols-1 items-start gap-4 lg:grid-cols-2" ] (List.map (\process -> ( process.id, processView saving user model process )) processes)
        , p [ class "text-sm text-slate-500" ]
            [ text "N/A excluye el paso del avance y conserva su estado. «!» indica un cambio de otro usuario: el primer clic lo reconoce y el siguiente cambia la casilla." ]
        ]


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
            , totalItem (String.fromInt closed) "cerrados por Rolando"
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


processView : SaveState -> User -> Model -> Process -> Html Msg
processView saving user model process =
    details [ class "min-w-0 rounded-xl border border-slate-200 bg-white shadow-sm" ]
        [ summary [ class "cursor-pointer break-words rounded-xl p-3 focus-visible:outline focus-visible:outline-2 focus-visible:outline-indigo-600" ]
            [ span [ class "font-semibold text-slate-900" ] [ text process.fields.client ]
            , span [ class "ml-3 text-sm text-slate-500" ] [ text (Domain.processLabel process.fields.kind) ]
            , span [ class "mt-2 block text-sm text-slate-600" ]
                [ text ("Inicio: " ++ dateLabel process.fields.startDate ++ " · " ++ progressLabel process) ]
            , case process.closure of
                PersonallyClosed _ ->
                    span [ class "mt-2 inline-block rounded bg-amber-100 px-2 py-1 text-sm text-amber-900" ] [ text "Cerrado por Rolando" ]

                Open ->
                    text ""
            ]
        , div [ class "space-y-3 border-t border-slate-100 p-2 sm:p-3" ]
            [ optionalText process.fields.context
            , closureView process
            , progressView process
            , nextStep process
            , legacyView process.legacySummary
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
                            ]
                        ]
                    , tbody [] (List.map (taskRow saving user model process) (Domain.tasks process.fields.kind))
                    ]
                ]
            ]
        ]


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
                [ strong [] [ text "Cerrado por Rolando" ]
                , p [] [ text reason ]
                , p [] [ text "El cierre personal se conserva. Los pasos pendientes siguen por confirmar y el avance se calcula por separado." ]
                ]


nextStep : Process -> Html Msg
nextStep process =
    if process.closure /= Open then
        text ""

    else
        let
            pending =
                Domain.tasks process.fields.kind
                    |> List.filter (\task -> (Domain.taskState task.id process).applicability == Applicable Todo)

            description =
                case List.head pending of
                    Just task ->
                        "Sigue: " ++ task.label ++ " · " ++ task.owner

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
taskRow saving user (Model editor _ tooltip) process task =
    let
        current =
            Domain.taskState task.id process
    in
    tr []
        ([ td [ class "border-b border-slate-100 py-1 align-top" ]
            [ fieldBox saving user editor process task ApplicabilityBox ]
         , td [ class "border-b border-slate-100 px-1 py-1 align-top" ]
            [ taskName tooltip process task
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
            ++ List.map (ownerCell saving user editor process task) [ "dot", "Rolando", "Otros" ]
        )


ownerHeading : String -> Html Msg
ownerHeading owner =
    th
        [ scope "col"
        , class
            ((case owner of
                "Rolando" ->
                    "w-14"

                "dot" ->
                    "w-8"

                _ ->
                    "w-10"
             )
                ++ " border-b border-slate-200 py-2 text-center text-sm font-medium text-slate-600"
            )
        ]
        [ text owner ]


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
                ++ (if reviewNeeded then
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
            , disabled (saving == Saving || editor /= Browsing || (control.blocked && not reviewNeeded))
            , attribute "aria-label" description
            , interaction
            ]
            []
        , if reviewNeeded then
            span [ class "text-sm font-bold leading-none text-amber-700", attribute "aria-hidden" "true" ] [ text "!" ]

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
            , label = "Confirmado por " ++ task.owner ++ ". "
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
            -- Owner columns total 8rem (2 + 3.5 + 2.5); keep the tooltip inside the card.
            div [ class "absolute left-0 top-full z-30 pt-1", Attr.style "width" "calc(100% + 8rem)" ]
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


escapeKey : Decode.Decoder Msg
escapeKey =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Escape" then
                    Decode.succeed CloseTip

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
