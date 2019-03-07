module DemoMenu exposing (main)

import Browser
import Browser.Events as Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Menu as Mu


type alias Model =
    Mu.Model Int


type Msg
    = SelectUp
    | SelectDown
    | NoOp


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map
            (\key ->
                case key of
                    "ArrowUp" ->
                        SelectUp

                    "ArrowDown" ->
                        SelectDown

                    _ ->
                        NoOp
            )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( Mu.init 3 [ 1, 2, 3, 4, 5 ], Cmd.none )
        , view =
            \model ->
                div []
                    [ h4 []
                        [ text
                            (Mu.getSelected model
                                |> Maybe.map String.fromInt
                                |> Maybe.withDefault "default"
                            )
                        ]
                    , ul []
                        (Mu.render
                            (\selected i ->
                                li
                                    (if selected then
                                        [ style "color" "red" ]

                                     else
                                        []
                                    )
                                    [ text <| String.fromInt i ]
                            )
                            model
                        )
                    ]
        , update =
            \msg model ->
                ( case msg of
                    SelectUp ->
                        Mu.selectBackward model

                    SelectDown ->
                        Mu.selectForward model

                    NoOp ->
                        model
                , Cmd.none
                )
        , subscriptions = \_ -> Events.onKeyDown keyDecoder
        }
