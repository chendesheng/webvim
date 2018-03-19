module View exposing (..)

import Html exposing (..)
import Model exposing (..)
import Internal.TextBuffer as B
import Array
import Html.Attributes exposing (..)
import Position exposing (Position)


view : Model -> Html msg
view { mode, cursor, lines, continuation } =
    let
        ( y, x ) =
            cursor

        statusBar =
            getStatusBar mode
    in
        div [ class "buffer" ]
            [ div [ class "line-numbers-container" ]
                [ div [ class "line-numbers" ]
                    (lines
                        |> B.countLines
                        |> List.range 1
                        |> List.map
                            (\i ->
                                div [ class "line-number" ]
                                    [ text <| toString i ]
                            )
                    )
                ]
            , div [ class "lines-container" ]
                [ div [ class "lines" ]
                    ((lines
                        |> B.mapLines
                            (\line ->
                                div [ class "line" ] [ text line ]
                            )
                        |> Array.toList
                     )
                        ++ if statusBar.cursor == Nothing then
                            [ renderCursor cursor ]
                           else
                            []
                    )
                ]
            , div
                [ class "status" ]
                [ div [] [ text statusBar.text ]
                , statusBar.cursor
                    |> Maybe.map renderCursor
                    |> Maybe.withDefault (text "")
                , div [ class "status-cmds" ] [ text continuation ]
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
                    |> ((*) 1.5)
                    |> toString
                )
                    ++ "rem"
              )
            ]
        ]
        []
