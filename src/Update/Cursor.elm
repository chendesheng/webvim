module Update.Cursor exposing
    ( correctCursor
    , correctPosition
    , correctPositionOnSurrogate
    , cursorScope
    , greaterTo
    , pairCursor
    , scrollTo
    , scrollToCursor
    )

import Internal.Brackets exposing (pairBracketAt)
import Internal.Position exposing (Position)
import Internal.Syntax exposing (Syntax)
import Internal.TextBuffer as B exposing (Patch(..))
import Model exposing (..)
import Update.Buffer as Buf
import Update.Motion exposing (setVisualEnd)


{-| scroll to ensure pos it is insdie viewport
-}
scrollTo : Int -> Int -> View -> View
scrollTo y lineHeight view =
    let
        miny =
            view.scrollTop

        maxy =
            miny + view.size.height - 1

        scrollTop =
            if miny > y then
                y

            else if y > maxy then
                y - maxy + miny

            else
                miny
    in
    Buf.setScrollTop scrollTop lineHeight view


scrollToCursor : Int -> View -> View
scrollToCursor lineHeight view =
    scrollTo (Tuple.first view.cursor) lineHeight view


correctCursor : Bool -> B.TextBuffer -> View -> View
correctCursor excludeLinkBreak lines view =
    Buf.setCursor
        (correctPosition view.cursor excludeLinkBreak lines)
        False
        view


greaterTo : Int -> Int -> Bool
greaterTo x y =
    y > x


correctPositionOnSurrogate : B.TextBuffer -> Position -> Position
correctPositionOnSurrogate lines (( y, x ) as pos) =
    lines
        |> B.getLine y
        |> Maybe.andThen
            (\line ->
                line
                    |> String.dropLeft (x - 1)
                    |> String.uncons
                    |> Maybe.map
                        (\( ch, _ ) ->
                            if
                                ch
                                    |> String.fromChar
                                    |> String.length
                                    |> greaterTo 1
                            then
                                if x + 1 >= String.length line - 1 then
                                    ( y, x - 1 )

                                else
                                    ( y, x + 1 )

                            else
                                pos
                        )
            )
        |> Maybe.withDefault pos


correctPosition : Position -> Bool -> B.TextBuffer -> Position
correctPosition pos excludeLineBreak lines =
    let
        ( y, x ) =
            pos

        y1 =
            0
                |> max (B.count lines - 2)
                |> min y

        maxcol =
            B.getLineMaxColumn y1 lines
                - (if excludeLineBreak then
                    String.length B.lineBreak

                   else
                    0
                  )

        x1 =
            maxcol
                |> max 0
                |> min x
    in
    if y1 == y && x == x1 then
        correctPositionOnSurrogate lines pos

    else
        correctPositionOnSurrogate lines ( y1, x1 )


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
            if remainderBy lineHeight view.scrollTopPx > 0 then
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
            Buf.setCursor ( y1, x1 ) True view

    else
        case Buf.cursorLineFirst lines y1 of
            Just cur ->
                Buf.setCursor cur True view

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
