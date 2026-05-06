port module Ports.Scratch exposing (..)

import Json.Decode as Decode


port fetchScratch : () -> Cmd msg


port fetchScratchSuccess : (Maybe Decode.Value -> msg) -> Sub msg


port fetchScratchFailure : (String -> msg) -> Sub msg
