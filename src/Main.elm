port module Main exposing (..)

import Model exposing (..)
import Update.Message exposing (..)
import Platform as P
import Update exposing (..)
import Html
import View exposing (..)
import Helper.KeySub exposing (downs)
import Window exposing (resizes)
import Helper.Debounce exposing (onDebounce, decodeEvent, DebounceEvent)
import Json.Decode as Decode exposing (decodeValue)
import List


-- This is the first line written in webvim-elm :)


tokenizeRequestParser : Decode.Decoder TokenizeRequest
tokenizeRequestParser =
    Decode.map4
        (\path version line lines ->
            { path = path
            , version = version
            , line = line
            , lines = lines
            }
        )
        (Decode.field "path" Decode.string)
        (Decode.field "version" Decode.int)
        (Decode.field "line" Decode.int)
        (Decode.field "lines" Decode.string)


handleTokenizeBounce : DebounceEvent -> Msg
handleTokenizeBounce event =
    event.payloads
        |> List.filterMap
            (decodeValue tokenizeRequestParser
                >> Result.toMaybe
            )
        |> List.foldl
            (\result current ->
                if result.line >= current.line then
                    current
                else
                    result
            )
            { path = ""
            , version = 0
            , line = 0xFFFFFFFF
            , lines = ""
            }
        |> SendTokenize


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ downs PressKey
                    , resizes Resize
                    , onDebounce <|
                        decodeEvent
                            (\resp ->
                                case resp of
                                    Ok event ->
                                        case event.action of
                                            "lint" ->
                                                SendLint

                                            "tokenize" ->
                                                handleTokenizeBounce event

                                            _ ->
                                                NoneMessage

                                    _ ->
                                        NoneMessage
                            )
                    ]
        }
