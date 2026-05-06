port module Ports.Auth exposing (..)

import Json.Decode as Decode
import Types exposing (SessionInfo)


port signIn : { email : String, password : String } -> Cmd msg


port signUp : { email : String, password : String } -> Cmd msg


port onSignUpSuccess : (Decode.Value -> msg) -> Sub msg


port onSignUpFailure : (String -> msg) -> Sub msg


port initiateGetUser : () -> Cmd msg


port gotUser : (Maybe Decode.Value -> msg) -> Sub msg


port onLoginSuccess : (Decode.Value -> msg) -> Sub msg


port onLoginFailure : (String -> msg) -> Sub msg


port logout : () -> Cmd msg


port loggedOut : ({} -> msg) -> Sub msg
