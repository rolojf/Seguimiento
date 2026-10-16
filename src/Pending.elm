module Pending exposing (Model, Msg, init, settle, update, view)

import Dict
import Domain exposing (Board, Change(..), Completion(..), Owner(..), PendingFields, SaveState(..), Target(..))
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E


type alias Model =
    { editor : Maybe ( Target, PendingFields ), submitting : SaveState, error : Maybe String }


type Msg
    = Add
    | Edit String
    | TitleChanged String
    | OwnerChanged String
    | Save
    | Cancel
    | Retry
    | Toggle String


init : Model
init =
    { editor = Nothing, submitting = Ready, error = Nothing }


update : Msg -> Board -> Model -> ( Model, Maybe Domain.Change )
update msg board model =
    case msg of
        Add ->
            ( { model | editor = Just ( New, { title = "", owner = Rolando, completion = Todo } ), error = Nothing }, Nothing )

        Edit id ->
            ( { model | editor = Dict.get id board.pending |> Maybe.map (\item -> ( Existing (Domain.ref item), item.fields )), error = Nothing }, Nothing )

        TitleChanged title ->
            ( changeForm (\fields -> { fields | title = title }) model, Nothing )

        OwnerChanged owner ->
            ( changeForm (\fields -> { fields | owner = parseOwner owner }) model, Nothing )

        Retry ->
            case model.editor of
                Just ( Existing previous, fields ) ->
                    case Dict.get previous.id board.pending of
                        Just current ->
                            let
                                refreshed =
                                    { fields | completion = current.fields.completion }
                            in
                            ( { model | editor = Just ( Existing (Domain.ref current), refreshed ), submitting = Saving }
                            , Just (SavePending (Existing (Domain.ref current)) refreshed)
                            )

                        Nothing ->
                            ( model, Nothing )

                _ ->
                    ( model, Nothing )

        Save ->
            ( { model | submitting = Saving }, model.editor |> Maybe.map (\( target, fields ) -> SavePending target fields) )

        Cancel ->
            ( init, Nothing )

        Toggle id ->
            ( model
            , Dict.get id board.pending
                |> Maybe.map
                    (\item ->
                        CompletePending (Domain.ref item)
                            (if item.fields.completion == Done then
                                Todo

                             else
                                Done
                            )
                    )
            )


changeForm : (PendingFields -> PendingFields) -> Model -> Model
changeForm change model =
    { model | editor = Maybe.map (\( target, fields ) -> ( target, change fields )) model.editor }


parseOwner : String -> Owner
parseOwner owner =
    case owner of
        "dot" ->
            Dot

        "Otros" ->
            Others

        _ ->
            Rolando


settle : Result String () -> Model -> Model
settle result model =
    case result of
        Ok _ ->
            if model.submitting == Saving then
                { model | editor = Nothing, submitting = Ready, error = Nothing }

            else
                model

        Err error ->
            { model | submitting = Ready, error = Just error }


view : SaveState -> Board -> Model -> Html Msg
view state board model =
    let
        items =
            Dict.values board.pending |> List.filter (\item -> not item.deleted) |> List.sortBy (\item -> ( item.createdAt, item.id ))

        locked =
            if model.editor /= Nothing then
                Saving

            else
                state

        count =
            List.filter (\item -> item.fields.completion == Todo) items |> List.length
    in
    Html.section [ A.id "pendientes", A.class "rounded-xl border border-slate-200 bg-white p-5 sm:p-7" ]
        [ Html.div [ A.class "mb-5 flex flex-wrap items-center justify-between gap-3" ]
            [ Html.div []
                [ Html.h2 [ A.class "text-xl font-semibold text-slate-900" ] [ Html.text "Pendientes" ]
                , Html.p [ A.class "text-sm text-slate-500" ] [ Html.text (String.fromInt count ++ " por resolver") ]
                ]
            , button locked Add "Añadir pendiente"
            ]
        , case model.editor of
            Just ( target, fields ) ->
                Html.div []
                    [ editor state target fields
                    , case ( model.error, target ) of
                        ( Just _, Existing _ ) ->
                            Html.div [ A.class "mb-5 text-sm text-slate-600" ]
                                [ Html.p [] [ Html.text "Revisa el registro actualizado en la lista antes de aplicar tu título y responsable." ]
                                , button state Retry "Aplicar sobre versión actual"
                                ]

                        _ ->
                            Html.text ""
                    ]

            Nothing ->
                Html.text ""
        , Html.div [ A.class "grid gap-5 md:grid-cols-3" ]
            (List.map (group locked items) [ Rolando, Dot, Others ])
        ]


group : SaveState -> List Domain.Pending -> Owner -> Html Msg
group state items owner =
    let
        owned =
            List.filter (\item -> item.fields.owner == owner) items
    in
    Html.div [ A.class "rounded-lg bg-slate-50 p-4" ]
        [ Html.h3 [ A.class "mb-3 font-semibold text-slate-700" ] [ Html.text (Domain.ownerLabel owner) ]
        , if List.isEmpty owned then
            Html.p [ A.class "text-sm text-slate-500" ] [ Html.text "Sin pendientes" ]

          else
            Html.ul [ A.class "space-y-3" ] (List.map (row state) owned)
        ]


row : SaveState -> Domain.Pending -> Html Msg
row state item =
    Html.li [ A.class "flex items-start gap-2" ]
        [ Html.label [ A.class "flex min-w-0 flex-1 items-start gap-2 text-sm" ]
            [ Html.input
                [ A.type_ "checkbox"
                , A.checked (item.fields.completion == Done)
                , A.disabled (state == Saving)
                , A.class "mt-1 h-4 w-4 shrink-0"
                , E.onCheck (\_ -> Toggle item.id)
                ]
                []
            , Html.span [ A.classList [ ( "break-words", True ), ( "text-slate-400 line-through", item.fields.completion == Done ) ] ]
                [ Html.text item.fields.title ]
            ]
        , Html.button
            [ A.type_ "button"
            , E.onClick (Edit item.id)
            , A.disabled (state == Saving)
            , A.class "text-xs font-medium text-blue-700 hover:underline"
            , A.attribute "aria-label" ("Editar " ++ item.fields.title)
            ]
            [ Html.text "Editar" ]
        ]


editor : SaveState -> Target -> PendingFields -> Html Msg
editor state target fields =
    Html.form [ E.onSubmit Save, A.class "mb-6 flex flex-wrap items-end gap-3 rounded-lg border border-slate-200 p-4" ]
        [ Html.label [ A.class "min-w-48 flex-1 text-sm font-medium" ]
            [ Html.text
                (case target of
                    New ->
                        "Nuevo pendiente"

                    Existing _ ->
                        "Editar pendiente"
                )
            , Html.input
                [ A.type_ "text"
                , A.required True
                , A.maxlength 90
                , A.value fields.title
                , E.onInput TitleChanged
                , A.disabled (state == Saving)
                , A.class "mt-1 block w-full rounded border border-slate-300 px-3 py-2"
                ]
                []
            ]
        , Html.label [ A.class "text-sm font-medium" ]
            [ Html.text "Responsable"
            , Html.select
                [ A.value (Domain.ownerLabel fields.owner)
                , E.onInput OwnerChanged
                , A.disabled (state == Saving)
                , A.class "mt-1 block rounded border border-slate-300 px-3 py-2"
                ]
                (List.map (\owner -> Html.option [ A.value (Domain.ownerLabel owner) ] [ Html.text (Domain.ownerLabel owner) ]) [ Rolando, Dot, Others ])
            ]
        , Html.button [ A.type_ "submit", A.disabled (state == Saving), A.class "rounded bg-slate-900 px-4 py-2 text-sm font-medium text-white disabled:opacity-50" ] [ Html.text "Guardar" ]
        , button state Cancel "Cancelar"
        ]


button : SaveState -> Msg -> String -> Html Msg
button state msg label =
    Html.button [ A.type_ "button", E.onClick msg, A.disabled (state == Saving), A.class "rounded border border-slate-300 px-3 py-2 text-sm font-medium hover:bg-slate-50 disabled:opacity-50" ] [ Html.text label ]
