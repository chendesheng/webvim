module Main exposing (..)

import Model exposing (..)
import Message exposing (..)
import Platform as P
import Update exposing (..)
import Html
import View exposing (..)
import KeySub exposing (downs)
import Window exposing (resizes)
import Persistent exposing (restoreBuffer)
import Message exposing (..)


-- This is the first line written in webvim-elm :)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ downs (PressKey 0)
                    , resizes Resize
                    , restoreBuffer Edit
                    ]
        }
