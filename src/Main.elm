module Main exposing (..)

import Model exposing (..)
import Message exposing (..)
import Platform as P
import Update exposing (..)


main : P.Program Never Model Msg
main =
    P.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
