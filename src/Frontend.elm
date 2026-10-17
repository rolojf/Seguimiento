module Frontend exposing (app)

import Browser
import Browser.Navigation as Nav
import Dashboard
import Dict
import Domain exposing (SaveState(..), User(..))
import File
import File.Download
import File.Select
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Lamdera
import Task
import Types exposing (ActiveSession, FrontendModel, FrontendMsg(..), LoginForm, LoginState(..), PrivateData, Theme(..), ToBackend(..), ToFrontend(..), Work(..))
import Url exposing (Url)


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    case model.login of
        SignedIn active ->
            Sub.map BoardMsg (Dashboard.subscriptions active.dashboard)

        _ ->
            Sub.none


init : Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init _ key =
    ( { key = key, login = CheckingSession, nextRequest = 1, theme = SystemTheme }, Cmd.none )


emptyForm : LoginForm
emptyForm =
    { username = "", password = "", error = Nothing }


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        ThemeChanged theme ->
            ( { model | theme = theme }, Cmd.none )

        UrlRequested (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        UrlRequested (Browser.External url) ->
            ( model, Nav.load url )

        UrlChanged _ ->
            ( model, Cmd.none )

        UsernameChanged username ->
            editForm (\form -> { form | username = username, error = Nothing }) model

        PasswordChanged password ->
            editForm (\form -> { form | password = password, error = Nothing }) model

        SignIn ->
            case model.login of
                SignedOut form ->
                    if String.isEmpty (String.trim form.username) || String.isEmpty form.password then
                        editForm (\current -> { current | error = Just "Escribe tu usuario y clave." }) model

                    else
                        ( { model | login = SigningIn form.username }
                        , Lamdera.sendToBackend (LogIn form.username form.password)
                        )

                _ ->
                    ( model, Cmd.none )

        SignOut ->
            case model.login of
                SignedIn _ ->
                    ( { model | login = SigningOut, nextRequest = model.nextRequest + 1 }, Lamdera.sendToBackend LogOut )

                _ ->
                    ( model, Cmd.none )

        _ ->
            case model.login of
                SignedIn active ->
                    updateBoard msg active model

                _ ->
                    ( model, Cmd.none )


updateBoard : FrontendMsg -> ActiveSession -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateBoard msg active model =
    case msg of
        BoardMsg action ->
            if active.work /= Nothing && not (Dashboard.isLocal action) then
                ( model, Cmd.none )

            else
                let
                    ( dashboard, cmd, change ) =
                        Dashboard.update action active.data.board active.dashboard

                    updated =
                        { active | dashboard = dashboard }
                in
                case change of
                    Just mutation ->
                        ( { model | login = SignedIn { updated | work = Just (ChangeRequest model.nextRequest) }, nextRequest = model.nextRequest + 1 }
                        , Cmd.batch [ Cmd.map BoardMsg cmd, Lamdera.sendToBackend (ChangeBoard model.nextRequest mutation) ]
                        )

                    Nothing ->
                        ( { model | login = SignedIn updated }, Cmd.map BoardMsg cmd )

        ImportBackup ->
            if active.work == Nothing then
                ( { model | nextRequest = model.nextRequest + 1 }
                , File.Select.file [ "application/json" ] (BackupSelected model.nextRequest)
                )

            else
                ( model, Cmd.none )

        BackupSelected request file ->
            if request /= model.nextRequest - 1 || active.work /= Nothing then
                ( model, Cmd.none )

            else if File.size file > 5 * 1024 * 1024 then
                ( { model | login = SignedIn { active | dashboard = Dashboard.notice (Err "El respaldo supera el límite de 5 MB.") active.dashboard } }, Cmd.none )

            else
                ( { model | login = SignedIn { active | work = Just (ImportRequest request) } }
                , Task.perform (BackupLoaded request) (File.toString file)
                )

        BackupLoaded request source ->
            if active.work == Just (ImportRequest request) then
                ( model, Lamdera.sendToBackend (ImportBoard request source) )

            else
                ( model, Cmd.none )

        ExportBackup ->
            if active.work == Nothing then
                ( { model | login = SignedIn { active | work = Just (ExportRequest model.nextRequest) }, nextRequest = model.nextRequest + 1 }
                , Lamdera.sendToBackend (ExportBoard model.nextRequest)
                )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


editForm : (LoginForm -> LoginForm) -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
editForm change model =
    case model.login of
        SignedOut form ->
            ( { model | login = SignedOut (change form) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        SessionStatus Nothing ->
            case model.login of
                SignedOut _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | login = SignedOut emptyForm, nextRequest = model.nextRequest + 1 }, Cmd.none )

        SessionStatus (Just data) ->
            case model.login of
                SigningOut ->
                    ( model, Lamdera.sendToBackend LogOut )

                SignedIn active ->
                    let
                        dashboard =
                            if active.data.user /= data.user then
                                Dashboard.init

                            else if active.work /= Nothing then
                                Dashboard.settle (Err "Conexión restablecida. Revisa los datos antes de repetir el cambio.") active.dashboard

                            else
                                active.dashboard
                    in
                    ( { model | login = SignedIn { data = data, dashboard = dashboard, work = Nothing } }, Cmd.none )

                _ ->
                    ( { model | login = SignedIn { data = data, dashboard = Dashboard.init, work = Nothing } }, Cmd.none )

        LoginRejected ->
            case model.login of
                SigningIn username ->
                    ( { model | login = SignedOut { emptyForm | username = username, error = Just "Usuario o clave incorrectos." } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        BoardChanged board ->
            case model.login of
                SignedIn active ->
                    ( { model | login = SignedIn (receiveBoard board active) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeResult request result ->
            case model.login of
                SignedIn active ->
                    if active.work == Just (ChangeRequest request) || active.work == Just (ImportRequest request) then
                        let
                            received =
                                Result.map (\board -> receiveBoard board active) result |> Result.withDefault active

                            dashboard =
                                if active.work == Just (ImportRequest request) then
                                    case result of
                                        Ok _ ->
                                            Dashboard.notice (Ok "Respaldo importado") Dashboard.init

                                        Err error ->
                                            Dashboard.notice (Err error) active.dashboard

                                else
                                    Dashboard.settle (Result.map (\_ -> ()) result) active.dashboard
                        in
                        ( { model | login = SignedIn { received | work = Nothing, dashboard = dashboard } }, Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ExportReady request source ->
            case model.login of
                SignedIn active ->
                    if active.work == Just (ExportRequest request) then
                        ( { model | login = SignedIn { active | work = Nothing, dashboard = Dashboard.notice (Ok "Respaldo preparado") active.dashboard } }
                        , File.Download.string "seguimiento-data.json" "application/json" source
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


receiveBoard : Domain.Board -> ActiveSession -> ActiveSession
receiveBoard board active =
    let
        data =
            active.data
    in
    -- A late acknowledgement must not replace a newer shared snapshot.
    if board.revision >= data.board.revision then
        { active | data = { data | board = board } }

    else
        active


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Seguimiento · Lamdera"
    , body =
        [ Html.div [ Attr.class "app-shell min-h-screen", Attr.attribute "data-theme" (themeValue model.theme) ]
            [ themeControl model.theme
            , case model.login of
                CheckingSession ->
                    viewStatus "Comprobando sesión…"

                SignedOut form ->
                    viewSignIn form

                SigningIn _ ->
                    viewStatus "Iniciando sesión…"

                SignedIn active ->
                    viewDashboard active

                SigningOut ->
                    viewStatus "Cerrando sesión…"
            ]
        ]
    }


themeControl : Theme -> Html FrontendMsg
themeControl theme =
    Html.div [ Attr.class "mx-auto flex max-w-7xl justify-end px-4 py-3 sm:px-6" ]
        [ Html.label [ Attr.class "flex items-center gap-2 text-sm text-slate-700" ]
            [ Html.text "Apariencia"
            , Html.select
                [ Attr.class "rounded border border-slate-300 bg-white px-3 py-2 text-slate-900"
                , Attr.value (themeValue theme)
                , Events.onInput
                    (\selected ->
                        ThemeChanged
                            (case selected of
                                "light" ->
                                    LightTheme

                                "dark" ->
                                    DarkTheme

                                _ ->
                                    SystemTheme
                            )
                    )
                ]
                [ Html.option [ Attr.value "system" ] [ Html.text "Sistema" ]
                , Html.option [ Attr.value "light" ] [ Html.text "Claro" ]
                , Html.option [ Attr.value "dark" ] [ Html.text "Oscuro" ]
                ]
            ]
        ]


themeValue : Theme -> String
themeValue theme =
    case theme of
        SystemTheme ->
            "system"

        LightTheme ->
            "light"

        DarkTheme ->
            "dark"


viewStatus : String -> Html msg
viewStatus message =
    Html.p [ Attr.attribute "role" "status", Attr.class "py-12 text-center text-gray-700" ]
        [ Html.text message ]


viewSignIn : LoginForm -> Html FrontendMsg
viewSignIn form =
    Html.main_ [ Attr.class "flex min-h-full flex-col justify-center px-4 py-12 sm:px-6 lg:px-8" ]
        [ Html.h1 [ Attr.class "text-center text-2xl font-bold leading-9 tracking-tight text-gray-900" ]
            [ Html.text "Seguimiento" ]
        , Html.div [ Attr.class "mt-8 w-full sm:mx-auto sm:max-w-md" ]
            [ Html.form
                [ Events.onSubmit SignIn
                , Attr.class "space-y-6 rounded-lg bg-white px-6 py-8 shadow sm:px-10"
                ]
                [ Html.div []
                    [ Html.label [ Attr.for "username", Attr.class "block text-base font-medium text-gray-900" ]
                        [ Html.text "Usuario" ]
                    , Html.input
                        [ Attr.id "username"
                        , Attr.name "username"
                        , Attr.type_ "text"
                        , Attr.attribute "autocomplete" "username"
                        , Attr.attribute "autocapitalize" "none"
                        , Attr.spellcheck False
                        , Attr.required True
                        , Attr.maxlength 64
                        , Attr.value form.username
                        , Attr.class inputClass
                        , Events.onInput UsernameChanged
                        ]
                        []
                    ]
                , Html.div []
                    [ Html.label [ Attr.for "password", Attr.class "block text-base font-medium text-gray-900" ]
                        [ Html.text "Clave de acceso" ]
                    , Html.input
                        [ Attr.id "password"
                        , Attr.name "password"
                        , Attr.type_ "password"
                        , Attr.attribute "autocomplete" "current-password"
                        , Attr.required True
                        , Attr.maxlength 43
                        , Attr.value form.password
                        , Attr.class inputClass
                        , Events.onInput PasswordChanged
                        ]
                        []
                    ]
                , case form.error of
                    Just error ->
                        Html.p [ Attr.attribute "role" "alert", Attr.class "text-base text-red-700" ]
                            [ Html.text error ]

                    Nothing ->
                        Html.text ""
                , Html.button
                    [ Attr.type_ "submit"
                    , Attr.class "w-full rounded-md bg-gray-900 px-4 py-2 font-semibold text-white hover:bg-gray-700 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-gray-900"
                    ]
                    [ Html.text "Entrar" ]
                ]
            ]
        ]


inputClass : String
inputClass =
    "mt-2 block w-full rounded-md border border-gray-300 px-3 py-2 text-gray-900 focus:border-gray-900 focus:outline-none focus:ring-1 focus:ring-gray-900"


viewDashboard : ActiveSession -> Html FrontendMsg
viewDashboard active =
    let
        state =
            if active.work == Nothing then
                Ready

            else
                Saving

        empty =
            Dict.isEmpty active.data.board.opportunities && Dict.isEmpty active.data.board.pending && Dict.isEmpty active.data.board.processes
    in
    Html.div [ Attr.class "min-h-screen bg-slate-50 text-slate-900" ]
        [ Html.header [ Attr.class "border-b border-slate-200 bg-white" ]
            [ Html.div [ Attr.class "mx-auto max-w-7xl px-4 py-5 sm:px-6" ]
                [ Html.div [ Attr.class "flex flex-wrap items-center justify-between gap-4" ]
                    [ Html.div []
                        [ Html.h1 [ Attr.class "text-2xl font-bold tracking-tight" ] [ Html.text "Seguimiento" ]
                        , Html.p [ Attr.class "mt-1 text-base text-slate-500" ] [ Html.text "Oportunidades, procesos y pendientes" ]
                        ]
                    , Html.div [ Attr.class "flex flex-wrap items-center gap-3 text-base" ]
                        [ Html.span [ Attr.class "text-slate-600" ] [ Html.text (userLabel active.data.user) ]
                        , Html.button [ Attr.type_ "button", Events.onClick SignOut, Attr.class "rounded border border-slate-300 px-3 py-2 hover:bg-slate-50" ] [ Html.text "Cerrar sesión" ]
                        ]
                    ]
                , Html.div [ Attr.class "mt-5 flex flex-wrap items-center justify-between gap-3" ]
                    [ Html.nav [ Attr.attribute "aria-label" "Secciones del tablero", Attr.class "flex flex-wrap gap-4 text-base font-medium text-blue-700" ]
                        [ Html.a [ Attr.href "#oportunidades" ] [ Html.text "Oportunidades" ]
                        , Html.a [ Attr.href "#procesos" ] [ Html.text "Procesos" ]
                        , Html.a [ Attr.href "#pendientes" ] [ Html.text "Pendientes" ]
                        ]
                    , Html.div [ Attr.class "flex flex-wrap gap-2" ]
                        [ Html.button
                            [ Attr.type_ "button"
                            , Events.onClick ImportBackup
                            , Attr.disabled (state == Saving || not empty)
                            , Attr.title "Importar un respaldo en un tablero vacío"
                            , Attr.class toolbarClass
                            ]
                            [ Html.text "Importar respaldo" ]
                        , Html.button [ Attr.type_ "button", Events.onClick ExportBackup, Attr.disabled (state == Saving), Attr.class toolbarClass ]
                            [ Html.text "Descargar respaldo" ]
                        ]
                    ]
                ]
            ]
        , if active.work /= Nothing then
            viewStatus "Guardando o preparando datos…"

          else
            Html.text ""
        , Html.map BoardMsg (Dashboard.view state active.data.user active.data.board active.dashboard)
        ]


toolbarClass : String
toolbarClass =
    "rounded border border-slate-300 px-3 py-2 text-base hover:bg-slate-50 disabled:cursor-not-allowed disabled:opacity-40"


userLabel : User -> String
userLabel user =
    case user of
        UserOne ->
            "usuario1"

        UserTwo ->
            "usuario2"
