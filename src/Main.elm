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


-- This is the first line written in webvim-elm :)


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
                        --vrView buf
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
                                                SendTokenize

                                            _ ->
                                                NoneMessage

                                    _ ->
                                        NoneMessage
                            )
                    ]
        }
