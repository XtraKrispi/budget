port module Ports.Definitions exposing (..)

import Types exposing (RawDefinition)


port fetchDefinitions : () -> Cmd msg


port fetchDefinitionsSuccess : (List RawDefinition -> msg) -> Sub msg


port fetchDefinitionsFailure : (String -> msg) -> Sub msg
