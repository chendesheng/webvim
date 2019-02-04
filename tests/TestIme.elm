module TestIme exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onClick)
import Font
    exposing
        ( FontInfo
        , emptyFontInfo
        , measureFont
        , renderMeasureDivs
        , stringWidth
        )
import Helper.Helper exposing (px, toCmd)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Ime
    exposing
        ( IME
        , IMEMsg
        , emptyIme
        , focusIme
        , renderIme
        , setImeActive
        , update
        )
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import Task


type Msg
    = NoOp
    | KeyPress String
    | IMEMessage IMEMsg
    | MeasureFont FontInfo
    | OnClick
    | StartLoad


type Model
    = Loading
    | Loaded
        { content : String
        , ime : IME
        , fontInfo : FontInfo
        }


css =
    node "link"
        [ property "rel" <| Encode.string "stylesheet"
        , property "href" <| Encode.string "/dist/style.min.css"
        ]
        []


main : Program () Model Msg
main =
    Browser.document
        { init =
            \_ ->
                ( Loading
                , Process.sleep 1000 |> Task.perform (always StartLoad)
                )
        , view =
            \model ->
                case model of
                    Loading ->
                        { title = "loading"
                        , body =
                            [ css
                            , renderMeasureDivs
                            ]
                        }

                    Loaded { content, ime, fontInfo } ->
                        { title = "test IME"
                        , body =
                            [ css
                            , span
                                [ style "whitespace" "pre"
                                , style "position" "relative"
                                , class "editor"
                                ]
                                [ text content
                                , div
                                    [ style "position" "absolute"
                                    , style "left"
                                        (px <|
                                            stringWidth fontInfo
                                                0
                                                (String.length content)
                                                content
                                        )
                                    , style "top" "0"
                                    , style "height" (px fontInfo.lineHeight)
                                    ]
                                    [ renderIme fontInfo ime |> Html.map IMEMessage ]
                                ]
                            ]
                        }
        , update =
            \msg model ->
                case model of
                    Loading ->
                        case msg of
                            StartLoad ->
                                ( model
                                , measureFont MeasureFont
                                )

                            MeasureFont fontInfo ->
                                let
                                    _ =
                                        Debug.log "MeasureFont" fontInfo
                                in
                                ( Loaded
                                    { ime = setImeActive True emptyIme
                                    , content = ""
                                    , fontInfo = fontInfo
                                    }
                                , Cmd.map IMEMessage focusIme
                                )

                            _ ->
                                ( model, Cmd.none )

                    Loaded m ->
                        case msg of
                            KeyPress s ->
                                ( Loaded
                                    { m
                                        | content = m.content ++ s
                                    }
                                , Cmd.none
                                )

                            IMEMessage imeMsg ->
                                let
                                    ( ime, cmd ) =
                                        update IMEMessage
                                            (KeyPress >> toCmd)
                                            imeMsg
                                            m.ime
                                in
                                ( Loaded { m | ime = ime }, cmd )

                            OnClick ->
                                ( model, Cmd.map IMEMessage focusIme )

                            _ ->
                                ( model, Cmd.none )
        , subscriptions = \_ -> onClick (Decode.succeed OnClick)
        }
