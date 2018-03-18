module View exposing (..)

import Html exposing (..)
import Model exposing (..)
import Internal.TextBuffer as B
import Array
import Html.Attributes exposing (..)
import Position exposing (Position)


view : Model -> Html msg
view { mode, cursor, lines } =
    let
        ( y, x ) =
            cursor

        statusBar =
            getStatusBar mode
    in
        div [ class "buffer" ]
            [ div [ class "lines" ]
                (lines
                    |> B.mapLines
                        (\line ->
                            div [ class "line" ] [ text line ]
                        )
                    |> Array.toList
                )
            , if statusBar.cursor == Nothing then
                renderCursor cursor
              else
                text ""
            , div
                [ class "status" ]
                [ div [] [ text statusBar.text ]
                , statusBar.cursor
                    |> Maybe.map renderCursor
                    |> Maybe.withDefault (text "")
                ]
            ]


renderCursor : Position -> Html msg
renderCursor ( y, x ) =
    div
        [ class "cursor"
        , style
            [ ( "left", (toString x) ++ "ch" )
            , ( "top"
              , (y
                    |> toFloat
                    |> ((*) 1.2)
                    |> toString
                )
                    ++ "rem"
              )
            ]
        ]
        []
