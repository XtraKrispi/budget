module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import HomePage
import Html exposing (..)
import LoginPage
import Navbar
import Ports.Auth as Auth exposing (initiateGetUser, loggedOut, logout)
import Route exposing (Route(..), routeParser)
import Types exposing (SessionInfo)
import Url
import Url.Parser



--TODO: There is a bug where logging in causes infinite rendering...
-- this does not trigger additional Cmds in Elm though


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


type Page
    = LoginPage LoginPage.Model
    | HomePage HomePage.Model
    | NotFoundPage


type Model
    = Initializing { key : Nav.Key, route : Route }
    | Initialized AppModel


type alias AppModel =
    { key : Nav.Key
    , route : Route
    , page : Page
    , session : Maybe SessionInfo
    }


routeInit : Nav.Key -> Route -> Maybe SessionInfo -> ( Model, Cmd Msg )
routeInit key route mSessionInfo =
    case ( route, mSessionInfo ) of
        ( ArchiveR, Just session ) ->
            let
                ( loginPageModel, loginPageCmd ) =
                    LoginPage.init
            in
            ( Initialized (AppModel key route (LoginPage loginPageModel) (Just session)), Cmd.map LoginPageMsg loginPageCmd )

        ( HomeR, Just session ) ->
            let
                ( homePageModel, homePageCmd ) =
                    HomePage.init
            in
            ( Initialized (AppModel key route (HomePage homePageModel) (Just session)), Cmd.map HomePageMsg homePageCmd )

        ( NotFoundR, _ ) ->
            ( Initialized (AppModel key route NotFoundPage Nothing), Cmd.none )

        ( LoginR, _ ) ->
            let
                ( loginPageModel, loginPageCmd ) =
                    LoginPage.init
            in
            ( Initialized (AppModel key route (LoginPage loginPageModel) Nothing), Cmd.batch [ Cmd.map LoginPageMsg loginPageCmd ] )

        _ ->
            let
                ( loginPageModel, loginPageCmd ) =
                    LoginPage.init
            in
            ( Initialized (AppModel key route (LoginPage loginPageModel) Nothing), Cmd.batch [ Nav.pushUrl key "/login", Cmd.map LoginPageMsg loginPageCmd ] )


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    case Url.Parser.parse routeParser url of
        Nothing ->
            ( Initialized
                { key = key
                , route = NotFoundR
                , page = NotFoundPage
                , session = Nothing
                }
            , Cmd.none
            )

        Just route ->
            ( Initializing { key = key, route = route }, initiateGetUser () )


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | LoginPageMsg LoginPage.Msg
    | HomePageMsg HomePage.Msg
    | GotUser (Maybe SessionInfo)
    | Logout
    | LoggedOut


getKeyAndSession : Model -> ( Nav.Key, Maybe SessionInfo )
getKeyAndSession model =
    case model of
        Initializing { key } ->
            ( key, Nothing )

        Initialized { key, session } ->
            ( key, session )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            let
                ( key, _ ) =
                    getKeyAndSession model
            in
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                ( key, session ) =
                    getKeyAndSession model
            in
            case Url.Parser.parse routeParser url of
                Nothing ->
                    ( Initialized (AppModel key NotFoundR NotFoundPage session), Cmd.none )

                Just val ->
                    routeInit key val session

        HomePageMsg hpm ->
            case model of
                Initialized appModel ->
                    case appModel.page of
                        HomePage mdl ->
                            let
                                ( newMdl, cmd ) =
                                    HomePage.update hpm mdl
                            in
                            ( Initialized { appModel | page = HomePage newMdl }, Cmd.map HomePageMsg cmd )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LoginPageMsg lpm ->
            case model of
                Initialized appModel ->
                    case appModel.page of
                        LoginPage mdl ->
                            let
                                ( newMdl, cmd ) =
                                    LoginPage.update lpm mdl
                            in
                            ( Initialized
                                { appModel
                                    | page = LoginPage newMdl
                                }
                            , Cmd.map LoginPageMsg cmd
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotUser user ->
            case model of
                Initializing { key, route } ->
                    routeInit key route user

                _ ->
                    ( model, Cmd.none )

        Logout ->
            ( model, Cmd.batch [ logout () ] )

        LoggedOut ->
            ( model, Nav.load "/" )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ loggedOut (\_ -> LoggedOut)
        , case model of
            Initializing _ ->
                Auth.gotUser GotUser

            Initialized appModel ->
                case appModel.page of
                    LoginPage mdl ->
                        Sub.map LoginPageMsg (LoginPage.subscriptions mdl)

                    HomePage mdl ->
                        Sub.map HomePageMsg (HomePage.subscriptions mdl)

                    NotFoundPage ->
                        Sub.none
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Budget"
    , body =
        case model of
            Initializing _ ->
                [ Html.div [] [] ]

            Initialized appModel ->
                case appModel.page of
                    LoginPage mdl ->
                        [ Html.map LoginPageMsg (LoginPage.view mdl) ]

                    HomePage mdl ->
                        [ Navbar.navbar Logout appModel.route, Html.map HomePageMsg (HomePage.view mdl) ]

                    NotFoundPage ->
                        [ Html.div [] [] ]
    }
