port module Ports.Scratch exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Types exposing (Scratch)


port fetchScratch : () -> Cmd msg


port fetchScratchSuccess : (Maybe Decode.Value -> msg) -> Sub msg


port fetchScratchFailure : (String -> msg) -> Sub msg


port insertScratch : { data : Encode.Value, userId : String } -> Cmd msg


port updateScratch : { data : Encode.Value, id : Int } -> Cmd msg


port saveScratchSuccess : (Int -> msg) -> Sub msg


port saveScratchFailure : (String -> msg) -> Sub msg
