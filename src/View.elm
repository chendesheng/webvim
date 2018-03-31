module View exposing (..)

import Html exposing (..)
import Model exposing (..)
import Internal.TextBuffer as B
import Array
import List
import Html.Attributes exposing (..)
import Position exposing (Position)


rem : number -> String
rem n =
    toString n ++ "rem"


ch : number -> String
ch n =
    toString n ++ "ch"


view : Model -> Html msg
view { mode, cursor, lines, continuation, view } =
    let
        ( y, x ) =
            cursor

        statusBar =
            getStatusBar mode

        translate x y =
            ( "transform"
            , "translate(" ++ ch x ++ ", " ++ rem y ++ ")"
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
                            ++ (if statusBar.cursor == Nothing then
                                    [ renderCursor cursor ]
                                else
                                    []
                               )
                            ++ (case mode of
                                    Visual tipe begin end ->
                                        [ div [ class "selections" ]
                                            (renderRange tipe begin end lines)
                                        ]

                                    Ex prefix _ ->
                                        case prefix of
                                            ExSearch _ range ->
                                                case range of
                                                    Just ( begin, end ) ->
                                                        [ div
                                                            [ class
                                                                "highlights"
                                                            ]
                                                            (renderRange
                                                                VisualRange
                                                                begin
                                                                end
                                                                lines
                                                            )
                                                        ]

                                                    _ ->
                                                        []

                                            _ ->
                                                []

                                    _ ->
                                        []
                               )
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
            [ ( "left", ch x )
            , ( "top", rem y )
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

        Visual tipe _ _ ->
            { text =
                case tipe of
                    VisualLine ->
                        "-- Visual Line --"

                    VisualBlock ->
                        "-- Visual Block --"

                    _ ->
                        "-- Visual --"
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


renderRange :
    VisualType
    -> Position
    -> Position
    -> B.TextBuffer
    -> List (Html msg)
renderRange tipe begin end lines =
    let
        ( by, bx ) =
            Basics.min begin end

        ( ey, ex ) =
            Basics.max begin end
    in
        List.range by ey
            |> List.map
                (\row ->
                    let
                        maxcol =
                            B.getLineMaxColumn row lines

                        ( a, b ) =
                            case tipe of
                                VisualLine ->
                                    ( 0, maxcol )

                                VisualBlock ->
                                    ( bx, ex )

                                _ ->
                                    if by == ey then
                                        ( bx, ex )
                                    else if row == by then
                                        ( bx, maxcol )
                                    else if row == ey then
                                        ( 0, ex )
                                    else
                                        ( 0, maxcol )
                    in
                        (div
                            [ style
                                [ ( "left", ch a )
                                , ( "top", rem row )
                                , ( "width", ch <| b - a + 1 )
                                ]
                            ]
                            []
                        )
                )
