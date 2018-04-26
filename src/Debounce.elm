port module Debounce exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode exposing (decodeValue)
import Time exposing (Time)


port debounce : Encode.Value -> Cmd msg


port onDebounce : (Encode.Value -> msg) -> Sub msg


type alias DebounceEvent =
    { action : String
    , payloads : List Encode.Value
    }


eventDecoder : Decode.Decoder DebounceEvent
eventDecoder =
    Decode.map2
        (\action payloads ->
            { action = action
            , payloads = payloads
            }
        )
        (Decode.field "action" Decode.string)
        (Decode.field "payloads" (Decode.list Decode.value))


decodeEvent :
    (Result String DebounceEvent -> msg)
    -> (Encode.Value -> msg)
decodeEvent =
    (>>) (decodeValue eventDecoder)


debounceLint : Time -> Cmd msg
debounceLint time =
    Encode.object
        [ ( "action", Encode.string "lint" )
        , ( "time", Encode.float time )
        ]
        |> debounce


debounceTokenize : Time -> Int -> Int -> String -> Cmd msg
debounceTokenize time version line lines =
    Encode.object
        [ ( "action", Encode.string "tokenize" )
        , ( "time", Encode.float time )
        , ( "payload"
          , Encode.object
                [ ( "line", Encode.int line )
                , ( "lines", Encode.string lines )
                , ( "version", Encode.int version )
                ]
          )
        ]
        |> debounce
