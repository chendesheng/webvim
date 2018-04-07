module View exposing (..)

import Html exposing (..)
import Html.Lazy exposing (..)
import Model exposing (..)
import Internal.TextBuffer as B
import Array
import List
import Html.Attributes exposing (..)
import Position exposing (Position)
import Vim.AST exposing (VisualType(..))
import Syntax exposing (Syntax)


rem : number -> String
rem n =
    toString n ++ "rem"


ch : number -> String
ch n =
    toString n ++ "ch"


translate : number -> number1 -> ( String, String )
translate x y =
    ( "transform"
    , "translate(" ++ ch x ++ ", " ++ rem y ++ ")"
    )


view : Model -> Html msg
view { mode, cursor, lines, syntax, continuation, view } =
    let
        statusBar =
            getStatusBar mode

        scrollTop =
            view.scrollTop

        { width, height } =
            view.size

        totalLines =
            B.countLines lines

        maybeCursor =
            case statusBar.cursor of
                Just _ ->
                    Nothing

                _ ->
                    case cursor of
                        ( y, x ) ->
                            Just ( y - scrollTop, x )
    in
        div [ class "editor" ]
            [ div [ class "buffer" ]
                [ lazy3 renderGutter
                    (scrollTop + 1)
                    (Basics.min (scrollTop + height + 1) totalLines)
                    totalLines
                , div [ class "lines-container" ]
                    (renderSelections scrollTop mode lines
                        ++ [ lazy3 renderLines
                                ( scrollTop, height + 1 )
                                lines
                                syntax
                           , renderCursor maybeCursor
                           ]
                    )
                ]
            , div
                [ class "status" ]
                [ div [] [ text statusBar.text ]
                , renderCursor statusBar.cursor
                , div [ class "status-cmds" ] [ text continuation ]
                ]
            ]


renderSelections : Int -> Mode -> B.TextBuffer -> List (Html msg)
renderSelections scrollTop mode lines =
    (case mode of
        Visual visual ->
            [ renderVisual
                scrollTop
                "selections"
                visual
                lines
            ]

        Ex { prefix, visual } ->
            let
                visual1 =
                    case visual of
                        Just v ->
                            case prefix of
                                ExSearch { match } ->
                                    case match of
                                        Just ( begin, end ) ->
                                            Just { v | end = begin }

                                        _ ->
                                            visual

                                _ ->
                                    visual

                        _ ->
                            Nothing

                visual2 =
                    case prefix of
                        ExSearch { match } ->
                            case match of
                                Just ( begin, end ) ->
                                    Just
                                        { tipe = VisualChars
                                        , begin = begin
                                        , end = end
                                        }

                                _ ->
                                    Nothing

                        _ ->
                            Nothing
            in
                (visual1
                    |> Maybe.map
                        (\v ->
                            [ renderVisual scrollTop
                                "selections"
                                v
                                lines
                            ]
                        )
                    |> Maybe.withDefault []
                )
                    ++ (visual2
                            |> Maybe.map
                                (\v ->
                                    [ renderVisual scrollTop
                                        "highlights"
                                        v
                                        lines
                                    ]
                                )
                            |> Maybe.withDefault []
                       )

        _ ->
            []
    )


renderCursor : Maybe Position -> Html msg
renderCursor cursor =
    case cursor of
        Just ( y, x ) ->
            div
                [ class "cursor"
                , style
                    [ ( "left", ch x )
                    , ( "top", rem y )
                    ]
                ]
                []

        _ ->
            text ""


getStatusBar : Mode -> { text : String, cursor : Maybe Position }
getStatusBar mode =
    case mode of
        Normal ->
            { text = "-- Normal --"
            , cursor = Nothing
            }

        Visual { tipe } ->
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

        Ex { exbuf } ->
            { text = B.toString exbuf.lines
            , cursor = Just exbuf.cursor
            }


renderVisual :
    Int
    -> String
    -> VisualMode
    -> B.TextBuffer
    -> Html msg
renderVisual scrollTop classname { tipe, begin, end } lines =
    div
        [ class classname ]
        (renderRange scrollTop tipe begin end lines)


renderRange :
    Int
    -> VisualType
    -> Position
    -> Position
    -> B.TextBuffer
    -> List (Html msg)
renderRange scrollTop tipe begin end lines =
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
                                , ( "top", rem (row - scrollTop) )
                                , ( "width", ch <| b - a + 1 )
                                ]
                            ]
                            []
                        )
                )


renderLines : ( Int, Int ) -> B.TextBuffer -> Syntax -> Html msg
renderLines ( scrollTop, height ) lines syntax =
    div
        [ class "lines"

        --, style [ translate 0 -scrollTop ]
        ]
        (if syntax.lang == "" then
            lines
                |> B.mapLinesToList scrollTop
                    (scrollTop + height)
                    (\line ->
                        div [ class "line" ] [ text line ]
                    )
         else
            syntax.lines
                |> Array.slice scrollTop
                    (Basics.min
                        (scrollTop + height)
                        (B.countLines lines)
                    )
                |> Array.map
                    (\sline ->
                        let
                            ( scopes, _ ) =
                                sline
                        in
                            div [ class "line" ]
                                (List.map
                                    (\scope ->
                                        let
                                            ( cls, s ) =
                                                scope
                                        in
                                            span [ class cls ] [ text s ]
                                    )
                                    scopes
                                )
                    )
                |> Array.toList
        )


renderGutter : Int -> Int -> Int -> Html msg
renderGutter begin end total =
    div [ class "gutter-container" ]
        [ div
            [ class "gutter"
            , style [ ( "width", total |> toString |> String.length |> ch ) ]
            ]
            (List.range begin end
                |> List.map
                    (\i ->
                        div [ class "line-number" ]
                            [ text <| toString i ]
                    )
            )
        ]
