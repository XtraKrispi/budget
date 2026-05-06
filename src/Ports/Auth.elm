port module Ports.Auth exposing (..)


type alias SessionInfo =
    { email : String
    , userId : String
    , confirmed : Bool
    }


port signIn : { email : String, password : String } -> Cmd msg


port signUp : { email : String, password : String } -> Cmd msg


port onSignUpSuccess :
    (SessionInfo
     -> msg
    )
    -> Sub msg


port onSignUpFailure : (String -> msg) -> Sub msg


port initiateGetUser : () -> Cmd msg


port gotUser : (Maybe SessionInfo -> msg) -> Sub msg


port onLoginSuccess : (SessionInfo -> msg) -> Sub msg


port onLoginFailure : (String -> msg) -> Sub msg
