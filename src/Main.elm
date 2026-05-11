module Main exposing (main)

import Admin.DefinitionsPage as DefinitionsPage
import ArchivePage
import Browser
import Browser.Hash
import Browser.Navigation as Nav
import BusinessLogic exposing (sessionInfoDecoder)
import Date exposing (Date, fromCalendarDate, today)
import HomePage
import Html exposing (..)
import Json.Decode as Decode
import LoginPage
import Navbar
import Ports.Auth as Auth exposing (initiateGetUser, loggedOut, logout)
import Route exposing (Route(..), routeParser)
import Task
import Time exposing (Month(..))
import Types exposing (SessionInfo)
import Url
import Url.Parser


main : Program () Model Msg
main =
    Browser.Hash.application
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
    | DefinitionsPage DefinitionsPage.Model
    | ArchivePage ArchivePage.Model
    | NotFoundPage


type Model
    = FetchingToday { key : Nav.Key, route : Route }
    | Initializing { key : Nav.Key, route : Route, today : Date }
    | Initialized AppModel


type alias AppModel =
    { key : Nav.Key
    , route : Route
    , page : Page
    , session : Maybe SessionInfo
    , today : Date
    }


routeInit : Nav.Key -> Route -> Date -> Maybe SessionInfo -> ( Model, Cmd Msg )
routeInit key route today mSessionInfo =
    case ( route, mSessionInfo ) of
        ( ArchiveR, Just session ) ->
            let
                ( archivePageModel, archivePageCmd ) =
                    ArchivePage.init
            in
            ( Initialized (AppModel key route (ArchivePage archivePageModel) (Just session) today), Cmd.map ArchivePageMsg archivePageCmd )

        ( HomeR, Just session ) ->
            let
                ( homePageModel, homePageCmd ) =
                    HomePage.init today
            in
            ( Initialized (AppModel key route (HomePage homePageModel) (Just session) today), Cmd.map HomePageMsg homePageCmd )

        ( DefinitionAdminR, Just session ) ->
            let
                ( defModel, defCmd ) =
                    DefinitionsPage.init today
            in
            ( Initialized (AppModel key route (DefinitionsPage defModel) (Just session) today), Cmd.map DefinitionsPageMsg defCmd )

        ( NotFoundR, _ ) ->
            ( Initialized (AppModel key route NotFoundPage Nothing today), Cmd.none )

        ( LoginR, _ ) ->
            let
                ( loginPageModel, loginPageCmd ) =
                    LoginPage.init
            in
            ( Initialized (AppModel key route (LoginPage loginPageModel) Nothing today), Cmd.batch [ Cmd.map LoginPageMsg loginPageCmd ] )

        _ ->
            let
                ( loginPageModel, loginPageCmd ) =
                    LoginPage.init
            in
            ( Initialized (AppModel key route (LoginPage loginPageModel) Nothing today), Cmd.batch [ Nav.pushUrl key "#/login", Cmd.map LoginPageMsg loginPageCmd ] )


defaultDate : Date
defaultDate =
    fromCalendarDate 1 Jan 1


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    case Url.Parser.parse routeParser url of
        Nothing ->
            ( Initialized
                { key = key
                , route = NotFoundR
                , page = NotFoundPage
                , session = Nothing
                , today = defaultDate
                }
            , Cmd.none
            )

        Just route ->
            ( FetchingToday { key = key, route = route }
            , Task.perform GotToday today
            )


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | GotToday Date
    | LoginPageMsg LoginPage.Msg
    | HomePageMsg HomePage.Msg
    | DefinitionsPageMsg DefinitionsPage.Msg
    | ArchivePageMsg ArchivePage.Msg
    | GotUser (Maybe SessionInfo)
    | Logout
    | LoggedOut


getKeyAndSession : Model -> ( Nav.Key, Maybe SessionInfo )
getKeyAndSession model =
    case model of
        FetchingToday { key } ->
            ( key, Nothing )

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
                    ( Initialized (AppModel key NotFoundR NotFoundPage session defaultDate), Cmd.none )

                Just val ->
                    case model of
                        Initialized mdl ->
                            routeInit key val mdl.today session

                        Initializing mdl ->
                            routeInit key val mdl.today session

                        FetchingToday _ ->
                            ( model, Cmd.none )

        GotToday today ->
            case model of
                FetchingToday { key, route } ->
                    ( Initializing { key = key, route = route, today = today }, initiateGetUser () )

                _ ->
                    ( model, Cmd.none )

        HomePageMsg hpm ->
            case model of
                Initialized appModel ->
                    case ( appModel.page, appModel.session ) of
                        ( HomePage mdl, Just session ) ->
                            let
                                ( newMdl, cmd ) =
                                    HomePage.update session hpm mdl
                            in
                            ( Initialized { appModel | page = HomePage newMdl }, Cmd.map HomePageMsg cmd )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DefinitionsPageMsg dpm ->
            case model of
                Initialized appModel ->
                    case ( appModel.page, appModel.session ) of
                        ( DefinitionsPage mdl, Just session ) ->
                            let
                                ( newMdl, cmd ) =
                                    DefinitionsPage.update session dpm mdl
                            in
                            ( Initialized { appModel | page = DefinitionsPage newMdl }, Cmd.map DefinitionsPageMsg cmd )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ArchivePageMsg dpm ->
            case model of
                Initialized appModel ->
                    case ( appModel.page, appModel.session ) of
                        ( ArchivePage mdl, Just _ ) ->
                            let
                                ( newMdl, cmd ) =
                                    ArchivePage.update dpm mdl
                            in
                            ( Initialized { appModel | page = ArchivePage newMdl }, Cmd.map ArchivePageMsg cmd )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LoginPageMsg (LoginPage.LoginSucceeded sessionInfo) ->
            case model of
                Initialized mdl ->
                    let
                        ( key, _ ) =
                            getKeyAndSession model

                        ( newModel, cmd ) =
                            routeInit key HomeR mdl.today (Just sessionInfo)
                    in
                    ( newModel, Cmd.batch [ Nav.pushUrl key "#/", cmd ] )

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
                Initializing { key, route, today } ->
                    routeInit key route today user

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
            FetchingToday _ ->
                Sub.none

            Initializing _ ->
                Auth.gotUser
                    (\mVal ->
                        case mVal of
                            Nothing ->
                                GotUser Nothing

                            Just val ->
                                case Decode.decodeValue sessionInfoDecoder val of
                                    Ok session ->
                                        GotUser (Just session)

                                    Err _ ->
                                        GotUser Nothing
                    )

            Initialized appModel ->
                case appModel.page of
                    LoginPage mdl ->
                        Sub.map LoginPageMsg (LoginPage.subscriptions mdl)

                    HomePage mdl ->
                        Sub.map HomePageMsg (HomePage.subscriptions mdl)

                    DefinitionsPage mdl ->
                        Sub.map DefinitionsPageMsg (DefinitionsPage.subscriptions mdl)

                    ArchivePage mdl ->
                        Sub.map ArchivePageMsg (ArchivePage.subscriptions mdl)

                    NotFoundPage ->
                        Sub.none
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Budget"
    , body =
        case model of
            FetchingToday _ ->
                [ Html.div [] [] ]

            Initializing _ ->
                [ Html.div [] [] ]

            Initialized appModel ->
                case appModel.page of
                    LoginPage mdl ->
                        [ Html.map LoginPageMsg (LoginPage.view mdl) ]

                    HomePage mdl ->
                        [ Navbar.navbar Logout appModel.route, Html.map HomePageMsg (HomePage.view mdl) ]

                    DefinitionsPage mdl ->
                        [ Navbar.navbar Logout appModel.route, Html.map DefinitionsPageMsg (DefinitionsPage.view mdl) ]

                    ArchivePage mdl ->
                        [ Navbar.navbar Logout appModel.route, Html.map ArchivePageMsg (ArchivePage.view mdl) ]

                    NotFoundPage ->
                        [ Html.div [] [ Html.text "Not Found" ] ]
    }
