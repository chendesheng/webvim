module Motion
    exposing
        ( saveMotion
        , gotoLine
        , setVisualEnd
        , setVisualBegin
        , runMotion
        , motion
        , matchString
        )

import Model exposing (..)
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Buffer as Buf
import Position exposing (Position, positionMin)
import String
import PositionClass exposing (..)
import Regex as Re
import Jumps exposing (saveCursorPosition)
import Message exposing (Msg(..))
import TextObject exposing (wordUnderCursor)


setVisualBegin : Position -> Buffer -> Buffer
setVisualBegin pos buf =
    case buf.mode of
        Visual { tipe, begin, end } ->
            { buf
                | mode =
                    Visual
                        { tipe = tipe
                        , begin = pos
                        , end = end
                        }
            }

        Ex ({ visual } as ex) ->
            case visual of
                Just v ->
                    { buf
                        | mode =
                            Ex
                                { ex
                                    | visual =
                                        Just { v | begin = pos }
                                }
                    }

                _ ->
                    buf

        _ ->
            buf


setVisualEnd : Position -> Buffer -> Buffer
setVisualEnd pos buf =
    case buf.mode of
        Visual { tipe, begin, end } ->
            { buf
                | mode =
                    Visual
                        { tipe = tipe
                        , begin = begin
                        , end = pos
                        }
            }

        Ex ({ visual } as ex) ->
            case visual of
                Just v ->
                    { buf
                        | mode =
                            Ex
                                { ex
                                    | visual =
                                        Just { v | end = pos }
                                }
                    }

                _ ->
                    buf

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

                V.MatchString str ->
                    case str of
                        V.WordUnderCursor ->
                            let
                                s =
                                    buf
                                        |> wordStringUnderCursor
                                        |> Maybe.map Tuple.second
                                        |> Maybe.map Re.escape
                            in
                                case s of
                                    Just s1 ->
                                        { last
                                            | matchString =
                                                Just ( s1, mo.forward )
                                        }

                                    _ ->
                                        last

                        _ ->
                            case buf.mode of
                                Ex { prefix, exbuf } ->
                                    case prefix of
                                        ExSearch { forward, match } ->
                                            let
                                                s =
                                                    exbuf.lines
                                                        |> B.toString
                                                        |> String.dropLeft 1
                                            in
                                                { last
                                                    | matchString =
                                                        Just ( s, forward )
                                                }

                                        _ ->
                                            buf.last

                                _ ->
                                    buf.last

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


gotoMatchedString : V.MotionOption -> Buffer -> Maybe Position
gotoMatchedString mo buf =
    case buf.mode of
        Ex { prefix, exbuf } ->
            case prefix of
                ExSearch { match } ->
                    Maybe.map Tuple.first match

                _ ->
                    Nothing

        _ ->
            case buf.last.matchString of
                Just ( s, forward ) ->
                    let
                        forward1 =
                            if mo.forward then
                                forward
                            else
                                not forward
                    in
                        case
                            (matchString forward1
                                (Re.regex s |> Re.caseInsensitive)
                                buf.cursor
                                buf.lines
                            )
                        of
                            Just ( cursor, _ ) ->
                                Just cursor

                            _ ->
                                Nothing

                _ ->
                    Nothing


lastItemOf : (a -> Bool) -> List a -> Maybe a
lastItemOf pred list =
    let
        lastItemOfHelper : (a -> Bool) -> Maybe a -> List a -> Maybe a
        lastItemOfHelper pred prevItem list =
            case list of
                x :: xs ->
                    if pred x then
                        lastItemOfHelper pred (Just x) xs
                    else
                        prevItem

                _ ->
                    prevItem
    in
        lastItemOfHelper pred Nothing list


matchStringInner :
    Bool
    -> Re.Regex
    -> Position
    -> B.TextBuffer
    -> Maybe ( Position, Position )
matchStringInner forward re ( y, x ) lines =
    case B.getLine y lines of
        Just line ->
            if forward then
                let
                    s =
                        String.dropLeft (x + 1) line
                in
                    case Re.find (Re.AtMost 1) re s of
                        [ m ] ->
                            Just
                                ( ( y, m.index + x + 1 )
                                , ( y, m.index + String.length m.match + x )
                                )

                        _ ->
                            matchStringInner forward re ( y + 1, -1 ) lines
            else
                Re.find Re.All re line
                    |> lastItemOf
                        (\m ->
                            x < 0 || m.index < x
                        )
                    |> Maybe.map
                        (\m ->
                            Just
                                ( ( y, m.index )
                                , ( y, m.index + String.length m.match - 1 )
                                )
                        )
                    |> Maybe.withDefault
                        (matchStringInner forward re ( y - 1, -1 ) lines)

        _ ->
            Nothing


matchString :
    Bool
    -> Re.Regex
    -> Position
    -> B.TextBuffer
    -> Maybe ( Position, Position )
matchString forward re pos lines =
    case matchStringInner forward re pos lines of
        Just res ->
            Just res

        _ ->
            let
                ( y, _ ) =
                    pos

                n =
                    B.countLines lines
            in
                if forward then
                    matchStringInner forward
                        re
                        ( 0, -1 )
                        (B.sliceLines 0 (y + 1) lines)
                    --|> Debug.log "search hit bottom"
                else
                    matchStringInner forward
                        re
                        ( n - y - 1, -1 )
                        (B.sliceLines y n lines)
                        |> Maybe.map
                            (\rg ->
                                let
                                    ( ( y1, x1 ), ( y2, x2 ) ) =
                                        rg
                                in
                                    ( ( y1 + y, x1 ), ( y2 + y, x2 ) )
                            )



--|> Debug.log "search hit top"


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
                                |> B.getLineMaxColumn y1
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

                V.MatchString str ->
                    case str of
                        V.LastSavedString ->
                            gotoMatchedString mo buf

                        V.WordUnderCursor ->
                            buf
                                |> wordStringUnderCursor
                                |> Maybe.andThen
                                    (\res ->
                                        let
                                            ( begin, str ) =
                                                res

                                            s =
                                                Re.escape str
                                        in
                                            (matchString mo.forward
                                                (Re.regex s |> Re.caseInsensitive)
                                                begin
                                                buf.lines
                                            )
                                                |> Maybe.map
                                                    (\rg ->
                                                        let
                                                            last =
                                                                buf.last
                                                        in
                                                            Tuple.first rg
                                                    )
                                    )

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


wordStringUnderCursor : Buffer -> Maybe ( Position, String )
wordStringUnderCursor { cursor, lines, config } =
    lines
        |> wordUnderCursor config.wordChars cursor
        |> Maybe.map
            (\rg ->
                let
                    ( begin, end ) =
                        rg
                in
                    ( begin
                    , lines
                        |> B.substring begin end
                        |> B.toString
                    )
            )


isSaveColumn : V.MotionData -> Bool
isSaveColumn md =
    case md of
        V.VLineDelta _ ->
            False

        V.LineDelta _ ->
            False

        _ ->
            True


saveCursorAfterJump : V.MotionData -> Position -> Position -> Buffer -> Buffer
saveCursorAfterJump md cursorBefore cursorAfter buf =
    let
        isJump md =
            case md of
                V.LineNumber _ ->
                    True

                V.ViewTop ->
                    True

                V.ViewMiddle ->
                    True

                V.ViewBottom ->
                    True

                V.MatchString _ ->
                    True

                _ ->
                    False
    in
        if cursorBefore /= cursorAfter && isJump md then
            let
                jumps =
                    saveCursorPosition
                        { path = buf.path
                        , cursor = cursorAfter
                        }
                        buf.jumps
            in
                { buf | jumps = jumps }
        else
            buf


motion : V.MotionData -> V.MotionOption -> Buffer -> ( Buffer, Cmd Msg )
motion md mo buf =
    case runMotion md mo buf of
        Just cursor ->
            ( buf
                |> Buf.setCursor cursor (isSaveColumn md)
                |> setVisualEnd cursor
                |> saveMotion md mo
                |> saveCursorAfterJump md buf.cursor cursor
            , Cmd.none
            )

        Nothing ->
            ( saveMotion md mo buf, Cmd.none )
