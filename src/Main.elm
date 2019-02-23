module Main exposing (main, toModel)

import Boot
import Browser
import Browser.Dom as Dom
import Browser.Events as Events exposing (onResize)
import Debouncers exposing (DebounceMessage(..), debouncePersistentAll)
import Font exposing (measureFont, renderMeasureDivs)
import Helper.Debounce as Deb
import Html
import Json.Decode as Decode
import Model exposing (..)
import Platform as P
import Task
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
    let
        bootTranslator flags =
            { toMsg = BootMessage
            , toModel = Booting flags
            }
    in
    Browser.application
        { onUrlRequest = \_ -> NoneMessage
        , onUrlChange = \_ -> NoneMessage
        , init =
            \flags url key ->
                Boot.init (bootTranslator flags) flags.service url
        , view =
            \model ->
                case model of
                    Booting _ bootingModel ->
                        { title = "Initializing"
                        , body = [ Boot.view bootingModel ]
                        }

                    Ready state ->
                        page state
        , update =
            \msg model ->
                case model of
                    Booting flags bootModel ->
                        case msg of
                            BootMessage bootMessage ->
                                Boot.update
                                    (bootTranslator flags)
                                    (\theme fontInfo size serverArgs ->
                                        init flags theme fontInfo size serverArgs
                                            |> Tuple.mapFirst Ready
                                    )
                                    bootMessage
                                    bootModel

                            _ ->
                                ( model, Cmd.none )

                    Ready state ->
                        update msg state
                            |> Tuple.mapSecond
                                (\cmd ->
                                    let
                                        persistent _ =
                                            Cmd.batch
                                                [ cmd
                                                , debouncePersistentAll
                                                    Debouncing
                                                    state.debouncers
                                                    3000
                                                ]
                                    in
                                    case msg of
                                        PressKeys _ ->
                                            persistent ()

                                        MouseWheel _ _ _ ->
                                            persistent ()

                                        Resize _ ->
                                            persistent ()

                                        _ ->
                                            cmd
                                )
                            |> toModel
        , subscriptions =
            \model ->
                case model of
                    Booting _ _ ->
                        Sub.none

                    Ready _ ->
                        Sub.batch
                            [ Events.onClick (Decode.succeed FocusIme)
                            , Events.onResize (\w h -> Resize { width = w, height = h })
                            ]
        }
