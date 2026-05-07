port module Ports.Archive exposing (..)

import Json.Encode as Encode


port insertArchive : { data : Encode.Value, userId : String } -> Cmd msg


port insertArchiveSuccess : ({} -> msg) -> Sub msg


port insertArchiveFailure : (String -> msg) -> Sub msg


port fetchArchive : () -> Cmd msg


port fetchArchiveSuccess : (Encode.Value -> msg) -> Sub msg


port fetchArchiveFailure : (String -> msg) -> Sub msg
