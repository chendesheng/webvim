port module Main exposing (..)

import Model exposing (..)
import Update.Message exposing (..)
import Platform as P
import Update exposing (..)
import Html
import View exposing (page)
import Browser.Events exposing (onResize)
import Browser
import Helper.Debounce exposing (onDebounce, decodeEvent, DebounceEvent)
import Helper.KeyEvent exposing (decodeKeyboardEvent)
import Helper.Document exposing (onKeyDown)
import Json.Decode as Decode


-- This is the first line written in webvim-elm :)


toModel : ( Buffer, cmd ) -> ( Model, cmd )
toModel =
    Tuple.mapFirst Ready


main : Program Flags Model Msg
main =
    Browser.document
        { init = (\flags -> ( Booting, initCommand flags ))
        , view =
            (\model ->
                case model of
                    Booting ->
                        { title = "Initializing"
                        , body = [ Html.text "" ]
                        }

                    Crashed err ->
                        { title = "Oopse"
                        , body = [ Html.text err ]
                        }

                    Ready buf ->
                        page buf
            )
        , update =
            (\msg model ->
                case msg of
                    Boot (Ok flags) ->
                        init flags
                            |> toModel

                    Boot (Err err) ->
                        ( Crashed ("boot failed: " ++ err), Cmd.none )

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
                    [ onKeyDown
                        (Decode.decodeValue decodeKeyboardEvent
                            >> Result.map PressKey
                            >> Result.withDefault NoneMessage
                        )
                    , onResize (\w h -> Resize { width = w, height = h })
                    , onDebounce <|
                        decodeEvent
                            (\resp ->
                                case resp of
                                    Ok event ->
                                        case event.action of
                                            "lint" ->
                                                SendLint

                                            "tokenize" ->
                                                SendTokenize

                                            _ ->
                                                NoneMessage

                                    _ ->
                                        NoneMessage
                            )
                    ]
        }
