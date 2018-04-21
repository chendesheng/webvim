module View exposing (..)

import Html exposing (..)
import Html.Lazy exposing (..)
import Model exposing (..)
import Internal.TextBuffer as B
import Elm.Array as Array
import List
import Html.Attributes exposing (..)
import Position exposing (Position)
import Vim.AST exposing (VisualType(..))
import Syntax exposing (Syntax, Token)
import Message exposing (LocationItem)
import String
import Elm.Array exposing (Array)


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
view buf =
    let
        { mode, cursor, lines, syntax, continuation, view, history } =
            buf

        scrollTop =
            view.scrollTop

        { width, height } =
            view.size

        totalLines =
            B.countLines lines

        maybeCursor =
            case mode of
                Ex _ ->
                    Nothing

                _ ->
                    case cursor of
                        ( y, x ) ->
                            Just ( y - scrollTop, x )

        isBufferDirty =
            history.pending /= Nothing || history.savePoint /= history.version

        ( searchRange, scrollTop1 ) =
            case incrementSearch scrollTop height mode lines of
                Just ( r, t ) ->
                    ( Just r, t )

                Nothing ->
                    ( Nothing, scrollTop )
    in
        div [ class "editor" ]
            [ div [ class "buffer" ]
                [ lazy3 renderGutter
                    (scrollTop1 + 1)
                    (Basics.min (scrollTop1 + height + 1) totalLines)
                    totalLines
                , div [ class "lines-container" ]
                    (renderVisual scrollTop1 height mode searchRange lines
                        ?:: (searchRange
                                |> Maybe.map
                                    (lazy3 renderHighlights scrollTop1 lines)
                            )
                        ?:: renderLines
                                scrollTop1
                                (height + 1)
                                lines
                                syntax
                        :: lazy3 renderLint scrollTop1 lines buf.lintItems
                        :: renderCursor maybeCursor
                        :: renderTip scrollTop1
                            buf.lintItems
                            (Maybe.map
                                (\cur ->
                                    let
                                        ( y, x ) =
                                            cur
                                    in
                                        ( y + scrollTop1, x )
                                )
                                maybeCursor
                            )
                            view.showTip
                        ?:: []
                    )
                ]
            , renderStatusBar
                isBufferDirty
                mode
                continuation
                buf.lintErrorsCount
                buf.name
            ]


renderStatusBar : Bool -> Mode -> String -> Int -> String -> Html msg
renderStatusBar dirty mode continuation errorsCnt name =
    let
        statusBar =
            getStatusBar mode
    in
        div
            [ class "status"
            , classList [ ( "dirty", dirty ) ]
            ]
            [ div [] [ text statusBar.text ]
            , renderCursor statusBar.cursor
            , div [ class "status-cmds" ] [ text continuation ]
            , div [ class "filename" ] [ text name ]
            , div [ class "lint-status" ]
                [ i [ class "far fa-times-circle" ] []
                , text <| toString errorsCnt
                ]
            ]


incrementSearch :
    Int
    -> Int
    -> Mode
    -> B.TextBuffer
    -> Maybe ( VisualMode, Int )
incrementSearch scrollTop height mode lines =
    (case mode of
        Ex { prefix, visual } ->
            case prefix of
                ExSearch { match } ->
                    case match of
                        Just ( begin, end ) ->
                            let
                                by =
                                    Basics.min
                                        (Tuple.first begin)
                                        (Tuple.first end)
                            in
                                Just
                                    ( { tipe = VisualChars
                                      , begin = begin
                                      , end = end
                                      }
                                    , if by < scrollTop then
                                        by
                                      else if by > scrollTop + height - 1 then
                                        by - height + 1
                                      else
                                        scrollTop
                                    )

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing
    )


renderVisual :
    Int
    -> Int
    -> Mode
    -> Maybe VisualMode
    -> B.TextBuffer
    -> Maybe (Html msg)
renderVisual scrollTop height mode searchRange lines =
    (case mode of
        Visual visual ->
            Just <| lazy3 renderSelections scrollTop lines visual

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
            in
                (visual1
                    |> Maybe.map (lazy3 renderSelections scrollTop lines)
                )

        _ ->
            Nothing
    )


maybeToList : Maybe a -> List a
maybeToList mb =
    case mb of
        Just x ->
            [ x ]

        _ ->
            []


(?::) : Maybe a -> List a -> List a
(?::) item list =
    case item of
        Just x ->
            x :: list

        _ ->
            list
infixr 5 ?::


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


renderHighlights :
    Int
    -> B.TextBuffer
    -> VisualMode
    -> Html msg
renderHighlights scrollTop lines { tipe, begin, end } =
    div
        [ class "highlights" ]
        (renderRange scrollTop tipe begin end lines False)


renderSelections :
    Int
    -> B.TextBuffer
    -> VisualMode
    -> Html msg
renderSelections scrollTop lines { tipe, begin, end } =
    div
        [ class "selections" ]
        (renderRange scrollTop tipe begin end lines False)


renderTip :
    Int
    -> List LocationItem
    -> Maybe Position
    -> Bool
    -> Maybe (Html msg)
renderTip scrollTop items maybeCursor showTip =
    if showTip then
        maybeCursor
            |> Maybe.andThen
                (\cursor ->
                    let
                        distanceFrom ( y, x ) { region } =
                            let
                                ( y1, x1 ) =
                                    Tuple.first region
                            in
                                ( abs (y1 - y), abs (x1 - x) )

                        renderDetails top details =
                            div
                                [ style
                                    [ ( "top", rem top ) ]
                                , class "tip"
                                ]
                                [ text details ]
                    in
                        items
                            |> List.filter
                                (\item ->
                                    let
                                        ( begin, end ) =
                                            item.region
                                    in
                                        cursor >= begin && cursor <= end
                                )
                            |> List.sortBy (distanceFrom cursor)
                            |> List.head
                            |> Maybe.map
                                (\item ->
                                    renderDetails
                                        (Tuple.first cursor - scrollTop + 1)
                                        --(by - scrollTop)
                                        (item.overview
                                            ++ B.lineBreak
                                            ++ item.details
                                        )
                                )
                )
    else
        Nothing


renderLint :
    Int
    -> B.TextBuffer
    -> List LocationItem
    -> Html msg
renderLint scrollTop lines items =
    let
        render scrollTop lines item =
            let
                ( begin, end ) =
                    item.region

                by =
                    Tuple.first begin
            in
                div
                    [ class "lint" ]
                    (renderRange scrollTop VisualChars begin end lines True)
    in
        div [ class "lints" ]
            (items
                |> List.filter
                    (\item ->
                        let
                            ( ( by, _ ), ( ey, _ ) ) =
                                item.region
                        in
                            not (ey < scrollTop || by >= scrollTop + 50)
                    )
                |> List.map
                    (render scrollTop lines)
            )


renderRange :
    Int
    -> VisualType
    -> Position
    -> Position
    -> B.TextBuffer
    -> Bool
    -> List (Html msg)
renderRange scrollTop tipe begin end lines excludeLineBreak =
    let
        ( by, bx ) =
            Basics.min begin end

        ( ey, ex ) =
            Basics.max begin end
    in
        List.range by ey
            |> List.filter (\i -> i >= scrollTop && i < scrollTop + 50)
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

                        width =
                            (b - a + 1)
                                - (if excludeLineBreak && b == maxcol then
                                    1
                                   else
                                    0
                                  )
                    in
                        (div
                            [ style
                                [ ( "left", ch a )
                                , ( "top", rem (row - scrollTop) )
                                , ( "width", ch width )
                                ]
                            ]
                            []
                        )
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


renderTokens : List Token -> String -> Int -> List (Html msg)
renderTokens spans line i =
    case spans of
        sp :: rest ->
            let
                j =
                    i + sp.length
            in
                (span
                    [ class sp.classname ]
                    [ line
                        |> String.slice i j
                        |> text
                    ]
                )
                    :: (renderTokens rest line j)

        _ ->
            []


renderLines : Int -> Int -> B.TextBuffer -> Syntax -> Html msg
renderLines scrollTop height lines syntax =
    div
        [ class "lines"

        --, style [ translate 0 -scrollTop ]
        ]
        (lines
            |> B.indexedMapLinesToList scrollTop
                (scrollTop + height)
                (\n line ->
                    case Array.get n syntax of
                        Just tokens ->
                            div [ class "line" ]
                                (renderTokens tokens line 0)

                        Nothing ->
                            div [ class "line" ] [ text line ]
                )
        )
