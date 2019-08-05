module Update.Cursor exposing
    ( cursorPoint
    , cursorScope
    , getScrollLeftPx
    , pairCursor
    , scroll
    )

import Font exposing (FontInfo, charWidth, cursorCharWidth, stringWidth)
import Internal.Brackets exposing (pairBracketAt)
import Internal.Position exposing (Position)
import Internal.Syntax exposing (Syntax)
import Internal.TextBuffer as B exposing (Patch(..))
import Model exposing (..)
import Model.Buffer exposing (..)
import Model.Global exposing (..)
import Model.View as View exposing (View)
import Update.Buffer as Buf
import Update.Motion exposing (setVisualEnd)
import Vim.AST as V


{-| move cursor ensure cursor is insdie viewport
-}
cursorScope : Int -> B.TextBuffer -> View -> View
cursorScope lineHeight lines view =
    let
        cursor =
            view.cursor

        ( y, x ) =
            cursor

        scrollTop =
            if view.scrollTopOffsetPx > 0 then
                view.scrollTop + 1

            else
                view.scrollTop

        maxy =
            min
                (scrollTop + view.size.height - 1)
                (max 0 (B.count lines - 2))

        miny =
            min scrollTop maxy

        y1 =
            y |> min maxy |> max miny

        x1 =
            lines
                |> B.getLine y
                |> Maybe.map
                    (\line ->
                        (String.length line - 2)
                            |> min x
                            |> max 0
                    )
                |> Maybe.withDefault 0
    in
    if y == y1 then
        if x == x1 then
            view

        else
            View.setCursor ( y1, x1 ) True view

    else
        case Buf.cursorLineFirst lines y1 of
            Just cur ->
                View.setCursor cur True view

            _ ->
                view


pairCursor : Mode -> B.TextBuffer -> Syntax -> View -> View
pairCursor mode lines syntax view =
    let
        cursor =
            case mode of
                Insert _ ->
                    view.cursor
                        |> Tuple.mapSecond
                            (\x -> Basics.max 0 (x - 1))

                _ ->
                    view.cursor
    in
    { view
        | matchedCursor =
            cursor
                |> pairBracketAt
                    view.scrollTop
                    (view.scrollTop + view.size.height)
                    lines
                    syntax
                |> Maybe.map (Tuple.pair cursor)
    }


cursorPoint : FontInfo -> B.TextBuffer -> Int -> Int -> ( Position, Position )
cursorPoint fontInfo lines y x =
    lines
        |> B.getLine y
        |> Maybe.map
            (\line ->
                let
                    x1 =
                        stringWidth fontInfo 0 x line

                    w =
                        cursorCharWidth fontInfo x line
                in
                ( ( y, x1 ), ( y, x1 + w ) )
            )
        |> Maybe.withDefault ( ( 0, 0 ), ( 0, 0 ) )


gutterWidth : B.TextBuffer -> Int
gutterWidth lines =
    let
        totalLines =
            B.count lines - 1
    in
    totalLines |> String.fromInt |> String.length


getScrollLeftPx : FontInfo -> Buffer -> Int
getScrollLeftPx fontInfo buf =
    let
        view =
            buf.view

        maybeCursor =
            case buf.mode of
                Ex _ ->
                    Nothing

                _ ->
                    Just view.cursor

        relativeGutterWidth =
            4
    in
    maybeCursor
        |> Maybe.map
            (\( y, x ) ->
                let
                    ( ( _, leftPx ), ( _, rightPx ) ) =
                        cursorPoint fontInfo buf.lines y x

                    widthPx =
                        toFloat view.size.width
                            - toFloat
                                (gutterWidth buf.lines
                                    + relativeGutterWidth
                                    + 1
                                )
                            * charWidth fontInfo '0'
                            |> floor
                in
                if leftPx < view.scrollLeftPx then
                    leftPx

                else if rightPx > widthPx + view.scrollLeftPx then
                    rightPx - widthPx

                else
                    view.scrollLeftPx
            )
        |> Maybe.withDefault 0


scroll : Maybe Int -> V.ScrollPosition -> Int -> Global -> View -> View
scroll count value lineCounts global view =
    let
        scope n =
            n
                |> max 0
                |> min (lineCounts - 1)

        setCursor : View -> View
        setCursor view_ =
            case count of
                Just n ->
                    case value of
                        V.ScrollBy _ ->
                            view_

                        _ ->
                            View.setCursor
                                ( scope (n - 1)
                                , Tuple.second view_.cursor
                                )
                                False
                                view_

                _ ->
                    view_

        scrollInner : View -> View
        scrollInner view_ =
            let
                size =
                    view_.size

                y =
                    Tuple.first view_.cursor

                y1 =
                    case value of
                        V.ScrollBy n ->
                            count
                                |> Maybe.withDefault 1
                                |> (*) n
                                |> (+) view_.scrollTop

                        V.ScrollToTop ->
                            y

                        V.ScrollToBottom ->
                            y - size.height - 2 + 1

                        V.ScrollToMiddle ->
                            y - (size.height - 2) // 2
            in
            View.setScrollTop (scope y1) view_
    in
    view
        |> setCursor
        |> scrollInner
