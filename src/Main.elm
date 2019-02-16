module Main exposing (main, toModel)

import Browser
import Browser.Events as Events exposing (onResize)
import Font exposing (measureFont, renderMeasureDivs)
import Helper.Debounce
    exposing
        ( DebounceEvent
        , debouncePersistentAll
        , decodeEvent
        , onDebounce
        )
import Html
import Json.Decode as Decode
import Model exposing (..)
import Platform as P
import Update exposing (..)
import Update.Message exposing (..)
import View exposing (page)



-- This is the first line written in webvim-elm :)
-- 可以用中文输入法了！表情也可以输入了😄


toModel : ( Global, cmd ) -> ( Model, cmd )
toModel =
    Tuple.mapFirst Ready


main : Program Flags Model Msg
main =
    Browser.document
        { init = \flags -> ( Booting flags, initCommand flags )
        , view =
            \model ->
                case model of
                    Booting _ ->
                        { title = "Initializing"
                        , body = [ renderMeasureDivs ]
                        }

                    Crashed err ->
                        { title = "Oopse"
                        , body = [ Html.text err ]
                        }

                    Ready state ->
                        page state
        , update =
            \msg model ->
                case msg of
                    Boot (Ok flags) ->
                        ( Booting flags, measureFont MeasureFont )

                    MeasureFont fontInfo ->
                        case model of
                            Booting flags ->
                                init fontInfo flags
                                    |> toModel

                            _ ->
                                ( model, Cmd.none )

                    Boot (Err err) ->
                        ( Crashed ("boot failed: " ++ err), Cmd.none )

                    _ ->
                        case model of
                            Ready state ->
                                update msg state
                                    |> Tuple.mapSecond
                                        (\cmd ->
                                            if msg == PersistentAll then
                                                cmd

                                            else
                                                Cmd.batch
                                                    [ cmd
                                                    , debouncePersistentAll 3000
                                                    ]
                                        )
                                    |> toModel

                            _ ->
                                ( model, Cmd.none )
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Events.onClick (Decode.succeed FocusIme)
                    , Events.onResize (\w h -> Resize { width = w, height = h })
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

                                            "persistentAll" ->
                                                PersistentAll

                                            _ ->
                                                NoneMessage

                                    _ ->
                                        NoneMessage
                            )
                    ]
        }
