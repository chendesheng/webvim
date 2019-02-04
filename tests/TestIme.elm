module TestIme exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onClick)
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
import Model exposing (FontInfo, stringWidth)
import Task


emptyFontInfo =
    { name = "iosevka"
    , widths = []
    , lineHeight = 21
    , size = 16
    }


measureFont : Cmd Msg
measureFont =
    Task.map3
        (\v1 v2 v3 ->
            let
                w1 =
                    v1.scene.width

                w2 =
                    v2.scene.width

                w3 =
                    v3.scene.width
            in
            [ ( "HALF", w1 ), ( "FULL", w2 ), ( "EMOJI", w3 ) ]
        )
        (Dom.getViewportOf "measureFont1")
        (Dom.getViewportOf "measureFont2")
        (Dom.getViewportOf "measureFont3")
        |> Task.attempt
            (\result ->
                (case result of
                    Ok widths ->
                        { emptyFontInfo | widths = widths }

                    _ ->
                        emptyFontInfo
                )
                    |> MeasureFont
            )


type Msg
    = NoOp
    | KeyPress String
    | IMEMessage IMEMsg
    | MeasureFont FontInfo
    | OnClick


type Model
    = Loading
    | Loaded
        { content : String
        , ime : IME
        , fontInfo : FontInfo
        }


measureDiv : String -> String -> Html msg
measureDiv divId str =
    div
        [ id divId
        , style "position" "absolute"
        , style "left" "-999px"
        ]
        [ text str ]


main : Program () Model Msg
main =
    Browser.document
        { init =
            \_ ->
                ( Loading
                , Cmd.batch [ measureFont ]
                )
        , view =
            \model ->
                case model of
                    Loading ->
                        { title = "loading"
                        , body =
                            [ node "style"
                                []
                                [ text
                                    ("body {font-size:'"
                                        ++ px emptyFontInfo.size
                                        ++ "';font-family: '"
                                        ++ emptyFontInfo.name
                                        ++ "'}"
                                    )
                                ]
                            , measureDiv "measureFont1" "m"
                            , measureDiv "measureFont2" "中"
                            , measureDiv "measureFont3" "😄"
                            ]
                        }

                    Loaded { content, ime, fontInfo } ->
                        { title = "test IME"
                        , body =
                            [ node "style"
                                []
                                [ text
                                    ("body {font-size:"
                                        ++ px emptyFontInfo.size
                                        ++ ";font-family: "
                                        ++ emptyFontInfo.name
                                        ++ "}"
                                    )
                                ]
                            , span
                                [ style "whitespace" "pre"
                                , style "position" "relative"
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
                                    , class "cursor"
                                    ]
                                    [ renderIme ime |> Html.map IMEMessage ]
                                ]
                            ]
                        }
        , update =
            \msg model ->
                case model of
                    Loading ->
                        case msg of
                            MeasureFont fontInfo ->
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
                                let
                                    _ =
                                        Debug.log "KeyPress" ( s, m )
                                in
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
