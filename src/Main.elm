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


minLine : List TokenizeRequest -> Maybe TokenizeRequest
minLine items =
    case items of
        [ x ] ->
            Just x

        x :: xs ->
            List.foldl
                (\result current ->
                    if result.line >= current.line then
                        current
                    else
                        result
                )
                x
                items
                |> Just

        _ ->
            Nothing


handleTokenizeBounce : DebounceEvent -> Msg
handleTokenizeBounce event =
    event.payloads
        |> List.filterMap
            (decodeValue tokenizeRequestParser
                >> Result.toMaybe
            )
        |> minLine
        |> Maybe.map SendTokenize
        |> Maybe.withDefault NoneMessage


toModel : ( Buffer, cmd ) -> ( Model, cmd )
toModel =
    Tuple.mapFirst Ready


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = (initCommand >> ((,) Booting))
        , view =
            (\model ->
                case model of
                    Booting ->
                        Html.text ""

                    Crashed err ->
                        Html.text err

                    Ready buf ->
                        view buf
            )
        , update =
            (\msg model ->
                case msg of
                    Boot (Ok flags) ->
                        init flags
                            |> toModel

                    Boot (Err err) ->
                        ( Crashed ("boot failed: " ++ toString err), Cmd.none )

                    _ ->
                        case model of
                            Ready buf ->
                                update msg buf
                                    |> toModel

                            _ ->
                                ( model, Cmd.none )
            )
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
