port module Debounce exposing (..)

import Json.Encode as Encode
import Time exposing (Time)


port debounce : Encode.Value -> Cmd msg


port onDebounce : (String -> msg) -> Sub msg


debounceLint : Time -> Cmd msg
debounceLint time =
    Encode.object
        [ ( "action", Encode.string "lint" )
        , ( "time", Encode.float time )
        ]
        |> debounce
