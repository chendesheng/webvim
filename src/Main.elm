module Main exposing (..)

import Model exposing (..)
import Message exposing (..)
import Platform as P
import Update exposing (..)
import Html
import View exposing (..)
import Json.Encode as Encode
import KeySub exposing (downs)


main : Program Encode.Value Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \_ -> downs (PressKey 0)
        }
