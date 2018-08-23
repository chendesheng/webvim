port module Helper.Document exposing (..)

import Json.Encode as Encode


port onKeyDown : (Encode.Value -> msg) -> Sub msg
