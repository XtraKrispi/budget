port module Ports.Scratch exposing (..)

import Types exposing (RawScratch)


port fetchScratch : () -> Cmd msg


port fetchScratchSuccess : (Maybe RawScratch -> msg) -> Sub msg


port fetchScratchFailure : (String -> msg) -> Sub msg
