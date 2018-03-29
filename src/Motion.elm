module Motion
    exposing
        ( saveMotion
        , gotoLine
        , setVisualEnd
        , runMotion
        , motion
        )

import Model exposing (..)
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Buffer as Buf
import Position exposing (Position, positionMin)
import String
import PositionClass exposing (..)


setVisualEnd : Position -> Buffer -> Buffer
setVisualEnd pos buf =
    case buf.mode of
        Visual tipe begin end ->
            { buf | mode = Visual tipe begin buf.cursor }

        _ ->
            buf


saveMotion : V.MotionData -> V.MotionOption -> Buffer -> Buffer
saveMotion md mo buf =
    let
        last =
            buf.last

        last1 =
            case md of
                V.MatchChar ch before ->
                    { last
                        | matchChar =
                            Just
                                { char = ch
                                , before = before
                                , forward = mo.forward
                                }
                    }

                _ ->
                    buf.last
    in
        { buf | last = last1 }


findPositionInBuffer :
    V.MotionData
    -> V.MotionOption
    -> Int
    -> Int
    -> String
    -> String
    -> B.TextBuffer
    -> Maybe Position
findPositionInBuffer md mo y x_ pline wordChars lines =
    case B.getLine y lines of
        Just line_ ->
            let
                line =
                    if mo.forward then
                        pline ++ line_
                    else
                        line_ ++ pline

                x =
                    if mo.forward then
                        x_
                    else if String.isEmpty pline then
                        x_
                    else
                        x_ + String.length line_
            in
                case findPosition wordChars md mo line x of
                    Just x1 ->
                        Just
                            ( y
                            , if mo.forward then
                                x1 - String.length pline
                              else
                                x1
                            )

                    Nothing ->
                        if mo.crossLine then
                            if mo.forward then
                                findPositionInBuffer
                                    md
                                    mo
                                    (y + 1)
                                    x
                                    line
                                    wordChars
                                    lines
                            else
                                findPositionInBuffer
                                    md
                                    mo
                                    (y - 1)
                                    x
                                    line
                                    wordChars
                                    lines
                        else
                            Nothing

        _ ->
            Nothing


gotoLine : Int -> B.TextBuffer -> Maybe Position
gotoLine y lines =
    B.getLine y lines
        |> Maybe.andThen
            (\line ->
                (findPosition
                    ""
                    V.LineFirst
                    (V.motionOption "<]$-")
                    line
                    0
                )
                    |> Maybe.map ((,) y)
            )


runMotion : V.MotionData -> V.MotionOption -> Buffer -> Maybe Position
runMotion md mo buf =
    if B.isEmpty buf.lines then
        Nothing
    else
        let
            ( y, x ) =
                buf.cursor

            bottomLine buf =
                (min
                    (B.countLines buf.lines)
                    (buf.view.scrollTop + buf.view.size.height)
                )
                    - 1

            middleLine buf =
                (bottomLine buf + buf.view.scrollTop) // 2
        in
            case md of
                V.LineNumber n ->
                    gotoLine (n % (B.countLines buf.lines)) buf.lines

                V.LineDelta n ->
                    let
                        y1 =
                            (y + n)
                                |> max 0
                                |> min (B.countLines buf.lines - 1)

                        x1 =
                            buf.lines
                                |> Buf.getLineMaxColumn y1
                                |> min buf.cursorColumn
                    in
                        Just ( y1, x1 )

                V.ViewTop ->
                    gotoLine buf.view.scrollTop buf.lines

                V.ViewMiddle ->
                    gotoLine
                        (middleLine buf)
                        buf.lines

                V.ViewBottom ->
                    gotoLine
                        (bottomLine buf)
                        buf.lines

                V.RepeatMatchChar ->
                    case buf.last.matchChar of
                        Just { char, before, forward } ->
                            let
                                option =
                                    V.motionOption "<]$-"

                                option1 =
                                    { option
                                        | forward =
                                            if forward then
                                                mo.forward
                                            else
                                                not mo.forward
                                    }
                            in
                                findPositionInBuffer
                                    (V.MatchChar char before)
                                    option1
                                    y
                                    x
                                    ""
                                    buf.config.wordChars
                                    buf.lines

                        _ ->
                            Nothing

                _ ->
                    findPositionInBuffer md
                        mo
                        y
                        x
                        ""
                        buf.config.wordChars
                        buf.lines


isSaveColumn : V.MotionData -> Bool
isSaveColumn md =
    case md of
        V.VLineDelta _ ->
            False

        V.LineDelta _ ->
            False

        _ ->
            True


motion : V.MotionData -> V.MotionOption -> Buffer -> Buffer
motion md mo buf =
    case runMotion md mo buf of
        Just cursor ->
            buf
                |> Buf.setCursor cursor (isSaveColumn md)
                |> setVisualEnd cursor
                |> saveMotion md mo

        Nothing ->
            buf
                |> saveMotion md mo
