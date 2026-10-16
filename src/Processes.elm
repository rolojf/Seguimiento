module Processes exposing (Model, Msg, init, settle, update, view)

import Dict
import Domain exposing (Applicability(..), Board, Change(..), Closure(..), Completion(..), Process, ProcessFields, ProcessKind(..), Ref, SaveState(..), Task, TaskChange(..))
import Html exposing (Html, button, caption, details, div, form, h2, h3, input, label, option, p, progress, section, select, span, strong, summary, table, tbody, td, text, textarea, th, thead, tr)
import Html.Attributes as Attr exposing (attribute, checked, class, colspan, disabled, for, id, maxlength, placeholder, required, rows, scope, selected, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Html.Keyed as Keyed


type Model
    = Model Editor String



-- A single editor prevents an unrelated save from discarding another draft.


type Editor
    = Browsing
    | Creating Draft
    | EditingReason Ref String String


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
    | EditReason Ref String
    | SetReason String
    | SaveReason
    | RetryReason


init : Model
init =
    Model Browsing ""


update : Msg -> Board -> Model -> ( Model, Maybe Change )
update msg board (Model editor error) =
    case ( msg, editor ) of
        ( AddProcess, Browsing ) ->
            ( Model (Creating { client = "", kind = Nothing, startDate = "", context = "" }) "", Nothing )

        ( Cancel, _ ) ->
            ( init, Nothing )

        ( Edit field inputValue, Creating draft ) ->
            ( Model (Creating (editDraft field inputValue draft)) "", Nothing )

        ( Create, Creating draft ) ->
            case processFields draft of
                Ok fields ->
                    ( Model editor "", Just (CreateProcess fields) )

                Err message ->
                    ( Model editor message, Nothing )

        ( ChangeStatus processRef taskId change, Browsing ) ->
            ( Model editor "", Just (ChangeTask processRef taskId change) )

        ( EditReason processRef taskId, Browsing ) ->
            case Dict.get processRef.id board.processes of
                Just process ->
                    let
                        reason =
                            case (Domain.taskState taskId process).applicability of
                                NotApplicable _ note ->
                                    note

                                Applicable _ ->
                                    ""
                    in
                    ( Model (EditingReason processRef taskId reason) "", Nothing )

                Nothing ->
                    ( Model editor "El proceso ya no está disponible.", Nothing )

        ( SetReason note, EditingReason processRef taskId _ ) ->
            ( Model (EditingReason processRef taskId note) "", Nothing )

        ( SaveReason, EditingReason processRef taskId note ) ->
            ( Model editor "", Just (ChangeTask processRef taskId (ExcludeTask (String.trim note))) )

        ( RetryReason, EditingReason processRef taskId note ) ->
            case Dict.get processRef.id board.processes of
                Just process ->
                    let
                        currentRef =
                            Domain.ref process
                    in
                    ( Model (EditingReason currentRef taskId note) ""
                    , Just (ChangeTask currentRef taskId (ExcludeTask (String.trim note)))
                    )

                Nothing ->
                    ( Model editor "El proceso ya no está disponible.", Nothing )

        _ ->
            ( Model editor error, Nothing )


settle : Result String () -> Model -> Model
settle result (Model editor _) =
    case result of
        Ok () ->
            init

        Err error ->
            Model editor error


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


view : SaveState -> Board -> Model -> Html Msg
view saving board ((Model editor error) as model) =
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
            p [ class "rounded-lg bg-red-50 p-3 text-sm text-red-800", attribute "role" "alert" ] [ text error ]
        , case editor of
            Creating draft ->
                createForm saving draft

            _ ->
                text ""
        , if List.isEmpty processes then
            p [ class "rounded-xl border border-dashed border-slate-300 p-8 text-center text-slate-600" ]
                [ text "Todavía no hay procesos. Agrega uno con su cliente, tipo y fecha de inicio." ]

          else
            Keyed.node "div" [ class "space-y-4" ] (List.map (\process -> ( process.id, processView saving model process )) processes)
        , p [ class "text-sm text-slate-500" ]
            [ text "Los cambios se comparten entre tus dispositivos. No aplica excluye el paso del avance y permite restaurar su estado anterior." ]
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
        , p [ class "mt-3 text-xs text-slate-500" ]
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
        [ label [ for controlId, class "block text-sm font-medium text-slate-700" ] [ text title ]
        , control
        ]


processView : SaveState -> Model -> Process -> Html Msg
processView saving model process =
    details [ class "rounded-xl border border-slate-200 bg-white shadow-sm" ]
        [ summary [ class "cursor-pointer rounded-xl p-4 focus-visible:outline focus-visible:outline-2 focus-visible:outline-indigo-600" ]
            [ span [ class "font-semibold text-slate-900" ] [ text process.fields.client ]
            , span [ class "ml-3 text-sm text-slate-500" ] [ text (Domain.processLabel process.fields.kind) ]
            , span [ class "mt-2 block text-sm text-slate-600" ]
                [ text ("Inicio: " ++ dateLabel process.fields.startDate ++ " · " ++ progressLabel process) ]
            , case process.closure of
                PersonallyClosed _ ->
                    span [ class "mt-2 inline-block rounded bg-amber-100 px-2 py-1 text-xs text-amber-900" ] [ text "Cerrado por Rolando" ]

                Open ->
                    text ""
            ]
        , div [ class "space-y-4 border-t border-slate-100 p-4" ]
            [ optionalText process.fields.context
            , closureView process
            , progressView process
            , nextStep process
            , legacyView process.legacySummary
            , div [ class "overflow-x-auto" ]
                [ table [ class "w-full min-w-[620px] border-collapse text-sm" ]
                    [ caption [ class "sr-only" ]
                        [ text (Domain.processLabel process.fields.kind ++ " de " ++ process.fields.client ++ ". Cada paso tiene un solo responsable.") ]
                    , thead []
                        [ tr [] (List.map (\heading -> th [ scope "col", class "border-b border-slate-200 px-2 py-3 text-left font-medium text-slate-600" ] [ text heading ]) [ "Descripción", "dot", "Rolando", "Otros" ]) ]
                    , tbody [] (List.concatMap (taskRows saving model process) (Domain.tasks process.fields.kind))
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
            div [ class "space-y-1 rounded-lg bg-amber-50 p-3 text-sm text-amber-900" ]
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
        p [ class "rounded-lg bg-slate-50 p-3 text-sm text-slate-700" ] [ text description ]


legacyView : String -> Html Msg
legacyView legacy =
    if String.isEmpty legacy then
        text ""

    else
        details [ class "rounded-lg border border-slate-200 p-3 text-sm text-slate-600" ]
            [ summary [ class "cursor-pointer font-medium" ] [ text "Ver registro anterior conservado" ]
            , p [ class "mt-2 whitespace-pre-wrap" ] [ text legacy ]
            , p [ class "mt-2 text-xs" ] [ text "Este registro no confirma automáticamente los pasos de la plantilla." ]
            ]


taskRows : SaveState -> Model -> Process -> Task -> List (Html Msg)
taskRows saving (Model editor error) process task =
    let
        current =
            Domain.taskState task.id process

        locked =
            saving == Saving || editor /= Browsing

        processRef =
            Domain.ref process

        excluded =
            case current.applicability of
                NotApplicable _ _ ->
                    True

                _ ->
                    False

        controls =
            div [ class "mt-2 flex flex-wrap gap-2" ]
                [ if task.allowNotApplicable then
                    button
                        [ type_ "button"
                        , class secondaryButton
                        , disabled locked
                        , onClick
                            (ChangeStatus processRef
                                task.id
                                (if excluded then
                                    RestoreTask

                                 else
                                    ExcludeTask ""
                                )
                            )
                        , attribute "aria-label"
                            ((if excluded then
                                "Restaurar estado anterior: "

                              else
                                "Marcar no aplica: "
                             )
                                ++ task.label
                            )
                        ]
                        [ text
                            (if excluded then
                                "Restaurar estado anterior"

                             else
                                "No aplica"
                            )
                        ]

                  else
                    text ""
                , if task.allowNotApplicable then
                    button [ type_ "button", class secondaryButton, disabled locked, onClick (EditReason processRef task.id) ] [ text "Motivo de N/A" ]

                  else
                    text ""
                ]

        reasonEditor =
            case editor of
                EditingReason ref taskId reason ->
                    if ref.id == process.id && taskId == task.id then
                        [ tr [] [ td [ colspan 4, class "border-b border-slate-200 bg-indigo-50 p-3" ] [ reasonForm saving error process task reason ] ] ]

                    else
                        []

                _ ->
                    []
    in
    tr
        [ class
            (if excluded then
                "bg-slate-50 text-slate-500"

             else
                "text-slate-700"
            )
        ]
        (td [ class "w-3/5 border-b border-slate-100 px-2 py-3 align-top" ]
            [ details []
                [ summary [ class "cursor-pointer font-medium" ] [ text task.label ]
                , p [ class "mt-2" ] [ text task.detail ]
                , optionalText task.condition
                ]
            , case current.evidence of
                Just evidence ->
                    p [ class "mt-2 whitespace-pre-wrap text-xs text-emerald-800" ] [ strong [] [ text "Evidencia: " ], text evidence ]

                Nothing ->
                    text ""
            , case current.applicability of
                NotApplicable _ reason ->
                    optionalText reason

                _ ->
                    text ""
            , controls
            ]
            :: List.map (ownerCell saving editor process task) [ "dot", "Rolando", "Otros" ]
        )
        :: reasonEditor


ownerCell : SaveState -> Editor -> Process -> Task -> String -> Html Msg
ownerCell saving editor process task column =
    let
        current =
            (Domain.taskState task.id process).applicability

        group =
            if task.owner == "dot" || task.owner == "Rolando" then
                task.owner

            else
                "Otros"

        excluded =
            case current of
                NotApplicable _ _ ->
                    True

                _ ->
                    False
    in
    td [ class "border-b border-slate-100 px-2 py-3 align-top" ]
        [ if column /= group then
            text ""

          else
            div [ class "space-y-1" ]
                [ input
                    [ type_ "checkbox"
                    , class "h-5 w-5 accent-indigo-600"
                    , checked (current == Applicable Done)
                    , disabled (saving == Saving || editor /= Browsing || excluded)
                    , attribute "aria-label" (process.fields.client ++ ": " ++ task.label ++ " · " ++ task.owner)
                    , onCheck
                        (\done ->
                            ChangeStatus (Domain.ref process)
                                task.id
                                (SetCompletion
                                    (if done then
                                        Done

                                     else
                                        Todo
                                    )
                                )
                        )
                    ]
                    []
                , if column == "Otros" then
                    p [ class "text-xs" ] [ text task.owner ]

                  else
                    text ""
                , if excluded then
                    p [ class "text-xs text-slate-500" ] [ text "N/A" ]

                  else if process.closure /= Open && current == Applicable Todo then
                    p [ class "text-xs text-amber-800" ] [ text "Por confirmar" ]

                  else
                    text ""
                ]
        ]


reasonForm : SaveState -> String -> Process -> Task -> String -> Html Msg
reasonForm saving error process task reason =
    let
        controlId =
            "reason-" ++ process.id ++ "-" ++ task.id
    in
    form [ class "space-y-3", onSubmit SaveReason ]
        [ formField controlId
            ("Motivo de N/A para «" ++ task.label ++ "» (opcional)")
            (textarea [ id controlId, class fieldClass, rows 2, maxlength 600, value reason, disabled (saving == Saving), onInput SetReason ] [])
        , optionalText task.condition
        , p [ class "text-xs text-slate-600" ] [ text "No aplica excluye el paso del avance. Su estado anterior se conserva." ]
        , div [ class "flex flex-wrap gap-2" ]
            [ button [ type_ "submit", class primaryButton, disabled (saving == Saving) ] [ text "Guardar N/A" ]
            , if String.isEmpty error then
                text ""

              else
                button [ type_ "button", class secondaryButton, disabled (saving == Saving), onClick RetryReason ] [ text "Aplicar sobre versión actual" ]
            , button [ type_ "button", class secondaryButton, disabled (saving == Saving), onClick Cancel ] [ text "Cancelar" ]
            ]
        ]


optionalText : String -> Html Msg
optionalText content =
    if String.isEmpty content then
        text ""

    else
        p [ class "mt-2 whitespace-pre-wrap text-sm text-slate-600" ] [ text content ]


dateLabel : String -> String
dateLabel raw =
    case String.split "-" raw of
        [ year, month, day ] ->
            day ++ "/" ++ month ++ "/" ++ year

        _ ->
            raw


fieldClass : String
fieldClass =
    "block w-full rounded-lg border border-slate-300 bg-white px-3 py-2 text-sm text-slate-900 focus:border-indigo-500 focus:outline-none focus:ring-1 focus:ring-indigo-500 disabled:opacity-50"


primaryButton : String
primaryButton =
    "rounded-lg bg-indigo-600 px-3 py-2 text-sm font-medium text-white hover:bg-indigo-700 disabled:cursor-not-allowed disabled:opacity-50"


secondaryButton : String
secondaryButton =
    "rounded-lg border border-slate-300 bg-white px-3 py-2 text-sm text-slate-700 hover:bg-slate-50 disabled:cursor-not-allowed disabled:opacity-50"
