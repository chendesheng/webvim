module View exposing (..)

import Html exposing (..)
import Model exposing (..)
import Internal.TextBuffer as B
import Array
import Html.Attributes exposing (..)
import Position exposing (Position)


view : Model -> Html msg
view { mode, cursor, lines, continuation, view } =
    let
        ( y, x ) =
            cursor

        statusBar =
            getStatusBar mode

        translate x y =
            ( "transform"
            , "translate("
                ++ toString x
                ++ "ch, "
                ++ toString y
                ++ "rem)"
            )

        scrollTop =
            view.scrollTop
    in
        div [ class "editor" ]
            [ div [ class "buffer" ]
                [ div [ class "gutter-container" ]
                    [ div
                        [ class "gutter"
                        , style [ translate 0 -scrollTop ]
                        ]
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
                    [ div
                        [ class "lines"
                        , style [ translate 0 -scrollTop ]
                        ]
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
                    |> toString
                )
                    ++ "rem"
              )
            ]
        ]
        []


getStatusBar : Mode -> { text : String, cursor : Maybe Position }
getStatusBar mode =
    case mode of
        Normal ->
            { text = "-- Normal --"
            , cursor = Nothing
            }

        Visual _ _ ->
            { text = "-- Visual --"
            , cursor = Nothing
            }

        Insert ->
            { text = "-- Insert --"
            , cursor = Nothing
            }

        TempNormal ->
            { text = "-- (Insert) --"
            , cursor = Nothing
            }

        Ex prefix buffer ->
            { text = B.toString buffer.lines
            , cursor = Just buffer.cursor
            }
