port module Persistent exposing (..)

import Json.Encode as Encode
import Message exposing (BufferInfo)


port saveBuffer : Encode.Value -> Cmd msg


port restoreBuffer : (BufferInfo -> msg) -> Sub msg


port getBuffer : String -> Cmd msg
