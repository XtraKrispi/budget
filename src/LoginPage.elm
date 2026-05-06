module LoginPage exposing (..)

import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Ports.Auth as Auth exposing (SessionInfo, signIn, signUp)
import Ports.Dialog as Dialog
import Svg
import Svg.Attributes as SA


type alias Model =
    { registrationEmail : String
    , registrationPassword : String
    , registrationPasswordConfirmation : String
    , loginEmail : String
    , loginPassword : String
    }


init : ( Model, Cmd Msg )
init =
    ( { registrationEmail = ""
      , registrationPassword = ""
      , registrationPasswordConfirmation = ""
      , loginEmail = ""
      , loginPassword = ""
      }
    , Cmd.none
    )


type Msg
    = OpenDialog
    | SignUp
    | SignUpSucceeded
        { email : String
        , userId : String
        , confirmed : Bool
        }
    | SignUpFailed String
    | RegistrationEmailUpdated String
    | RegistrationPasswordUpdated String
    | RegistrationPasswordConfirmationUpdated String
    | LoginEmailUpdated String
    | LoginPasswordUpdated String
    | Login
    | LoginSucceeded SessionInfo
    | LoginFailed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenDialog ->
            ( { model | registrationEmail = "", registrationPassword = "", registrationPasswordConfirmation = "" }, Dialog.openDialog "registration-dialog" )

        SignUp ->
            ( model, signUp { email = model.registrationEmail, password = model.registrationPassword } )

        RegistrationEmailUpdated txt ->
            ( { model | registrationEmail = txt }, Cmd.none )

        RegistrationPasswordUpdated txt ->
            ( { model | registrationPassword = txt }, Cmd.none )

        RegistrationPasswordConfirmationUpdated txt ->
            ( { model | registrationPasswordConfirmation = txt }, Cmd.none )

        SignUpSucceeded str ->
            ( model, Cmd.none )

        SignUpFailed str ->
            ( model, Cmd.none )

        LoginEmailUpdated str ->
            ( { model | loginEmail = str }, Cmd.none )

        LoginPasswordUpdated str ->
            ( { model | loginPassword = str }, Cmd.none )

        Login ->
            ( model
            , signIn
                { email = model.loginEmail
                , password = model.loginPassword
                }
            )

        LoginSucceeded _ ->
            ( model, Nav.load "/" )

        LoginFailed err ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Auth.onSignUpSuccess SignUpSucceeded
        , Auth.onSignUpFailure SignUpFailed
        , Auth.onLoginSuccess LoginSucceeded
        , Auth.onLoginFailure LoginFailed
        ]


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "bg-base-200 min-h-screen min-w-screen flex flex-col" ]
        [ Html.div [ Attr.class "w-screen bg-base-400 min-h-50 flex justify-center items-center" ]
            [ Html.h1 [ Attr.class "text-8xl font-bold" ] [ Html.text "Budget" ]
            ]
        , Html.div [ Attr.class "hero grow h-full" ]
            [ Html.div [ Attr.class "hero-content flex-col lg:flex-row-reverse" ]
                [ Html.node "dialog"
                    [ Attr.class "modal", Attr.id "registration-dialog" ]
                    [ Html.div [ Attr.class "modal-box" ]
                        [ closeButton
                        , Html.h3 [ Attr.class "font-bold text-lg" ]
                            [ Html.text "Sign Up"
                            ]
                        , registrationForm model
                        ]
                    , Html.form [ Attr.method "dialog", Attr.class "modal-backdrop" ]
                        [ Html.button [] [ Html.text "close" ]
                        ]
                    ]
                , Html.div [ Attr.class "text-center lg:text-left" ]
                    [ Html.h1 [ Attr.class "text-5xl font-bold" ]
                        [ Html.text "Login now!"
                        ]
                    , Html.p [ Attr.class "pt-6" ] [ Html.text "Please log in to the Budget system to see what you have upcoming and to make any changes!" ]
                    , Html.p [ Attr.class "py-2" ] [ Html.text "Don't have an account? No problem! Sign up here!" ]
                    , Html.button [ Attr.class "btn btn-primary", Events.onClick OpenDialog ] [ Html.text "Sign Up" ]
                    ]
                , Html.form [ Attr.class "card bg-base-100 w-full max-w-sm shrink-0 shadow-2xl", Events.onSubmit Login ]
                    [ Html.div [ Attr.class "card-body" ]
                        [ Html.div [ Attr.class "form-control" ]
                            [ Html.label [ Attr.class "label" ]
                                [ Html.span [ Attr.class "label-text" ] [ Html.text "Email" ]
                                ]
                            , Html.input
                                [ Attr.class "input"
                                , Attr.type_ "email"
                                , Attr.placeholder "email"
                                , Attr.required True
                                , Attr.autofocus True
                                , Events.onInput LoginEmailUpdated
                                ]
                                []
                            ]
                        , Html.div [ Attr.class "form-control" ]
                            [ Html.label [ Attr.class "label" ]
                                [ Html.span [ Attr.class "label-text" ] [ Html.text "Password" ]
                                ]
                            , Html.input
                                [ Attr.class "input"
                                , Attr.type_ "password"
                                , Attr.placeholder "password"
                                , Attr.required True
                                , Events.onInput LoginPasswordUpdated
                                ]
                                []
                            , Html.label [ Attr.class "label" ] [ Html.a [ Attr.href "/" ] [ Html.text "Forgot password?" ] ]
                            ]
                        , Html.div [ Attr.class "form-control" ]
                            [ Html.button
                                [ Attr.class "btn btn-primary"
                                , Attr.type_ "submit"
                                ]
                                [ Html.text "Login" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


closeButton : Html msg
closeButton =
    Html.form [ Attr.method "dialog" ]
        [ Html.button [ Attr.class "btn btn-sm btn-circle btn-ghost absolute right-2 top-2" ]
            [ Svg.svg
                [ SA.class "h-6 w-6"
                , SA.fill "none"
                , SA.viewBox "0 0 24 24"
                ]
                [ Svg.path
                    [ SA.stroke "currentColor"
                    , SA.strokeLinecap "round"
                    , SA.strokeLinejoin "round"
                    , SA.strokeWidth "2"
                    , SA.d "M6 18 17.94 6M18 18 6.06 6"
                    ]
                    []
                ]
            ]
        ]


registrationForm : Model -> Html Msg
registrationForm model =
    Html.form [ Attr.class "flex flex-col space-y-2", Events.onSubmit SignUp ]
        [ Html.div [ Attr.class "form-control w-full max-w-xs mt-2" ]
            [ Html.input
                [ Attr.class "input w-full max-w-xs grow"
                , Attr.placeholder "Email"
                , Attr.type_ "email"
                , Attr.required True
                , Attr.value model.registrationEmail
                , Events.onInput RegistrationEmailUpdated
                ]
                []
            ]
        , Html.div []
            [ Html.div [ Attr.class "form-control w-full max-w-xs" ]
                [ Html.input
                    [ Attr.class "input w-full max-w-xs grow"
                    , Attr.placeholder "Password"
                    , Attr.type_ "password"
                    , Attr.required True
                    , Attr.value model.registrationPassword
                    , Events.onInput RegistrationPasswordUpdated
                    ]
                    []
                ]
            , Html.div [ Attr.class "form-control w-full max-w-xs mt-2" ]
                [ Html.input
                    [ Attr.class "input w-full max-w-xs grow"
                    , Attr.placeholder "Password Confirmation"
                    , Attr.type_ "password"
                    , Attr.required True
                    , Attr.value model.registrationPasswordConfirmation
                    , Events.onInput RegistrationPasswordConfirmationUpdated
                    ]
                    []
                ]
            , Html.div [ Attr.class "label", Attr.classList [ ( "invisible", model.registrationPassword == model.registrationPasswordConfirmation ) ] ]
                [ Html.span [ Attr.class "text-xs text-error" ] [ Html.text "Passwords do not match" ]
                ]
            ]
        , Html.div [ Attr.class "modal-action" ]
            [ Html.button
                [ Attr.class "btn btn-primary"
                , Attr.type_ "submit"
                , Attr.disabled (model.registrationPassword /= model.registrationPasswordConfirmation)
                ]
                [ Html.text "Sign Up" ]
            ]
        ]
