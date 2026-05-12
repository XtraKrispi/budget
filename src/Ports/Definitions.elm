port module Ports.Definitions exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


port fetchDefinitions : { includeDeleted : Bool } -> Cmd msg


port fetchDefinitionsSuccess : (Decode.Value -> msg) -> Sub msg


port fetchDefinitionsFailure : (String -> msg) -> Sub msg


port insertDefinition : { data : Encode.Value, userId : String } -> Cmd msg


port updateDefinition : { data : Encode.Value, id : Int } -> Cmd msg


port saveDefinitionSuccess : (Decode.Value -> msg) -> Sub msg


port saveDefinitionFailure : (String -> msg) -> Sub msg
