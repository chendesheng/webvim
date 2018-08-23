port module Helper.Debounce exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode exposing (decodeValue)
import Time as Time


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
    (Result Decode.Error DebounceEvent -> msg)
    -> (Decode.Value -> msg)
decodeEvent =
    (>>) (decodeValue eventDecoder)


debounceLint : Int -> Cmd msg
debounceLint time =
    Encode.object
        [ ( "action", Encode.string "lint" )
        , ( "time", Encode.int time )
        ]
        |> debounce


debounceTokenize : Int -> Cmd msg
debounceTokenize time =
    Encode.object
        [ ( "action", Encode.string "tokenize" )
        , ( "time", Encode.int time )
        ]
        |> debounce
