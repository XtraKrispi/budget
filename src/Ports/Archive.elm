port module Ports.Archive exposing (..)

import Json.Encode as Encode


port insertArchive : { data : Encode.Value, userId : String } -> Cmd msg


port insertArchiveSuccess : ({} -> msg) -> Sub msg


port insertArchiveFailed : (String -> msg) -> Sub msg
