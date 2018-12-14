port module Helper.Debounce exposing
    ( DebounceEvent
    , debounce
    , debounceLint
    , debouncePersistentAll
    , debounceTokenize
    , decodeEvent
    , eventDecoder
    , onDebounce
    )

import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode


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
debounceLint =
    debounceAction "lint"


debounceTokenize : Int -> Cmd msg
debounceTokenize =
    debounceAction "tokenize"


debouncePersistentAll : Int -> Cmd msg
debouncePersistentAll =
    debounceAction "persistentAll"


debounceAction : String -> Int -> Cmd msg
debounceAction action time =
    Encode.object
        [ ( "action", Encode.string action )
        , ( "time", Encode.int time )
        ]
        |> debounce
