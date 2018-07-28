module Update.Cursor exposing (..)

import Model exposing (..)
import Internal.TextBuffer as B exposing (Patch(..))
import Update.Buffer as Buf
import Internal.Position exposing (Position)
import Update.Motion exposing (setVisualEnd)
import Internal.Brackets exposing (pairBracketAt)


{-| scroll to ensure pos it is insdie viewport
-}
scrollTo : Int -> Buffer -> Buffer
scrollTo y ({ view, lines } as buf) =
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
                buf.view.scrollTop
    in
        Buf.setScrollTop scrollTop buf


scrollToCursor : Buffer -> Buffer
scrollToCursor buf =
    scrollTo (Tuple.first buf.cursor) buf


correctCursor : Buffer -> Buffer
correctCursor buf =
    Buf.setCursor
        (correctPosition buf.cursor False buf.lines)
        False
        buf


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
            pos
        else
            ( y1, x1 )


{-| move cursor ensure cursor is insdie viewport
-}
cursorScope : Buffer -> Buffer
cursorScope ({ view, cursor, lines } as buf) =
    let
        ( y, x ) =
            cursor

        scrollTop =
            if Basics.rem view.scrollTopPx buf.view.lineHeight > 0 then
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
                buf
            else
                Buf.setCursor ( y1, x1 ) True buf
        else
            case Buf.cursorLineFirst lines y1 of
                Just cursor ->
                    buf
                        |> Buf.setCursor cursor True
                        |> setVisualEnd cursor

                _ ->
                    buf


pairSource : Buffer -> Position
pairSource buf =
    case buf.mode of
        Insert _ ->
            buf.cursor
                |> Tuple.mapSecond
                    (\x -> Basics.max 0 (x - 1))

        _ ->
            buf.cursor


pairCursor : Buffer -> Buffer
pairCursor buf =
    Buf.updateView
        (\view ->
            let
                cursor =
                    pairSource buf
            in
                { view
                    | matchedCursor =
                        cursor
                            |> pairBracketAt
                                buf.view.scrollTop
                                (buf.view.scrollTop + buf.view.size.height)
                                buf.lines
                                buf.syntax
                            |> Maybe.map ((,) cursor)
                }
        )
        buf
