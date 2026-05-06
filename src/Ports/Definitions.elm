port module Ports.Definitions exposing (..)

import Json.Decode as Decode


port fetchDefinitions : () -> Cmd msg


port fetchDefinitionsSuccess : (Decode.Value -> msg) -> Sub msg


port fetchDefinitionsFailure : (String -> msg) -> Sub msg
