port module Main exposing (..)

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
import Debounce exposing (onDebounce, decodeEvent, DebounceEvent)
import Json.Decode as Decode exposing (decodeValue)
import List


-- This is the first line written in webvim-elm :)


decodeTuple : Decode.Decoder ( Int, String )
decodeTuple =
    Decode.map2
        (\x y -> ( x, y ))
        (Decode.field "line" Decode.int)
        (Decode.field "lines" Decode.string)


handleTokenizeBounce : DebounceEvent -> Msg
handleTokenizeBounce event =
    event.payloads
        |> List.filterMap
            (decodeValue decodeTuple
                >> Result.toMaybe
            )
        |> List.foldl
            (\result current ->
                let
                    a =
                        Tuple.first result

                    --|> Debug.log "a"
                    b =
                        Tuple.first current

                    --|> Debug.log "b"
                in
                    if a >= b then
                        current
                    else
                        result
            )
            ( 0xFFFFFFFF, "" )
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
                    , restoreBuffer Edit
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
