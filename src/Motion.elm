module Motion
    exposing
        ( saveMotion
        , gotoLine
        , setVisualEnd
        , setVisualBegin
        , runMotion
        , motion
        , matchString
        , wordStringUnderCursor
        , wORDStringUnderCursor
        )

import Model exposing (..)
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Buffer as Buf
import Position exposing (Position, positionMin)
import String
import PositionClass exposing (..)
import Regex as Re exposing (regex)
import Jumps exposing (saveJump)
import Message exposing (Msg(..))
import TextObject exposing (wordUnderCursor, wORDUnderCursor)
import Helper exposing (repeatfn)
import Brackets exposing (pairBracket, bracketsParser)
import Parser as P
import Elm.Array as Array


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


wholeWord : String -> String
wholeWord s =
    if Re.contains (regex "\\w") s then
        "\\b" ++ Re.escape s ++ "\\b"
    else
        Re.escape s


saveMotion : V.MotionData -> V.MotionOption -> Buffer -> Buffer -> Buffer
saveMotion md mo oldbuf buf =
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
                                    oldbuf
                                        |> wordStringUnderCursor
                                        |> Maybe.map (Tuple.second >> wholeWord)
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
    -> B.TextBuffer
    -> Maybe Position
findPositionInBuffer md mo y x wordChars lines =
    let
        findPositionInBufferInner md mo y x target line wordChars lines =
            case findPosition wordChars md mo target x of
                Just x1 ->
                    Just
                        ( y
                        , if mo.forward then
                            x1 - (String.length target - String.length line)
                          else
                            x1
                        )

                Nothing ->
                    if mo.crossLine then
                        if mo.forward then
                            case B.getLine (y + 1) lines of
                                Just nextLine ->
                                    findPositionInBufferInner
                                        md
                                        mo
                                        (y + 1)
                                        x
                                        (target ++ nextLine)
                                        nextLine
                                        wordChars
                                        lines

                                _ ->
                                    Nothing
                        else
                            case B.getLine (y - 1) lines of
                                Just nextLine ->
                                    findPositionInBufferInner
                                        md
                                        mo
                                        (y - 1)
                                        (x + String.length nextLine)
                                        (nextLine ++ target)
                                        nextLine
                                        wordChars
                                        lines

                                _ ->
                                    Nothing
                    else
                        Nothing
    in
        case B.getLine y lines of
            Just line ->
                findPositionInBufferInner md mo y x line line wordChars lines

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


gotoMatchedString : Maybe Int -> V.MotionOption -> Buffer -> Maybe Position
gotoMatchedString count mo buf =
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

                        re =
                            s
                                |> Re.regex
                                |> Re.caseInsensitive

                        findNext cursor =
                            Maybe.map Tuple.first
                                (matchString forward1
                                    re
                                    cursor
                                    buf.lines
                                )
                    in
                        repeatfn
                            (Maybe.withDefault 1 count)
                            findNext
                            buf.cursor

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
                    B.count lines
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


runMotion : Maybe Int -> V.MotionData -> V.MotionOption -> Buffer -> Maybe Position
runMotion count md mo buf =
    if B.isEmpty buf.lines then
        Nothing
    else
        let
            bottomLine buf =
                (min
                    (B.count buf.lines - 1)
                    (buf.view.scrollTop + buf.view.size.height)
                )
                    - 1

            middleLine buf =
                (bottomLine buf + buf.view.scrollTop) // 2
        in
            case md of
                V.BufferTop ->
                    case count of
                        Just n ->
                            gotoLine (n - 1) buf.lines

                        _ ->
                            gotoLine 0 buf.lines

                V.BufferBottom ->
                    case count of
                        Just n ->
                            gotoLine (n - 1) buf.lines

                        _ ->
                            gotoLine (B.count buf.lines - 2) buf.lines

                V.LineDelta ->
                    let
                        forward =
                            mo.forward

                        n =
                            if forward then
                                Maybe.withDefault 1 count
                            else
                                -(Maybe.withDefault 1 count)

                        ( y, x ) =
                            buf.cursor

                        y1 =
                            (y + n)
                                |> max 0
                                |> min (B.count buf.lines - 2)

                        x1 =
                            buf.lines
                                |> B.getLineMaxColumn y1
                                |> min buf.cursorColumn
                    in
                        Just ( y1, x1 )

                V.ViewTop ->
                    (buf.view.scrollTop + (Maybe.withDefault 0 count))
                        |> Basics.min (bottomLine buf)
                        |> flip gotoLine buf.lines

                V.ViewMiddle ->
                    gotoLine
                        (middleLine buf)
                        buf.lines

                V.ViewBottom ->
                    (bottomLine buf - (Maybe.withDefault 0 count))
                        |> Basics.max buf.view.scrollTop
                        |> flip gotoLine buf.lines

                V.RepeatMatchChar ->
                    case buf.last.matchChar of
                        Just { char, before, forward } ->
                            let
                                mo1 =
                                    (if mo.forward then
                                        { mo | forward = forward }
                                     else
                                        { mo | forward = not forward }
                                    )

                                findNext ( y, x ) =
                                    findPositionInBuffer
                                        (V.MatchChar char before)
                                        mo1
                                        y
                                        x
                                        buf.config.wordChars
                                        buf.lines
                            in
                                repeatfn (Maybe.withDefault 1 count) findNext buf.cursor

                        _ ->
                            Nothing

                V.MatchString str ->
                    case str of
                        V.LastSavedString ->
                            gotoMatchedString count mo buf

                        V.WordUnderCursor ->
                            buf
                                |> wordStringUnderCursor
                                --|> Debug.log "word under cursor"
                                |> Maybe.andThen
                                    (\res ->
                                        let
                                            ( begin, str ) =
                                                res

                                            re =
                                                wholeWord str
                                                    |> Re.regex
                                                    |> Re.caseInsensitive

                                            findNext cursor =
                                                Maybe.map Tuple.first
                                                    (matchString mo.forward
                                                        re
                                                        cursor
                                                        buf.lines
                                                    )
                                        in
                                            repeatfn
                                                (Maybe.withDefault 1 count)
                                                findNext
                                                buf.cursor
                                    )

                        _ ->
                            Nothing

                V.MatchPair ->
                    case count of
                        Just n ->
                            let
                                cnt =
                                    B.count buf.lines - 1
                            in
                                n
                                    |> toFloat
                                    |> flip (/) 100
                                    |> (*) (toFloat cnt)
                                    |> ceiling
                                    |> (\x -> x - 1)
                                    |> Basics.min (cnt - 1)
                                    |> flip gotoLine buf.lines

                        _ ->
                            let
                                ( y, x ) =
                                    buf.cursor
                            in
                                buf.lines
                                    |> B.getLine y
                                    |> Maybe.andThen
                                        (String.dropLeft x
                                            >> P.run bracketsParser
                                            >> Result.toMaybe
                                            >> Maybe.map (\dx -> ( y, x + dx ))
                                        )
                                    |> Maybe.andThen
                                        (pairBracket
                                            0
                                            (Array.length buf.syntax)
                                            buf.lines
                                            buf.syntax
                                        )

                _ ->
                    let
                        findNext ( y, x ) =
                            findPositionInBuffer md
                                mo
                                y
                                x
                                buf.config.wordChars
                                buf.lines
                    in
                        repeatfn
                            (Maybe.withDefault 1 count)
                            findNext
                            buf.cursor


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


wORDStringUnderCursor : Buffer -> Maybe ( Position, String )
wORDStringUnderCursor { cursor, lines } =
    lines
        |> wORDUnderCursor cursor
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
        V.VLineDelta ->
            False

        V.LineDelta ->
            False

        _ ->
            True


saveCursorBeforeJump : V.MotionData -> Position -> Buffer -> Buffer
saveCursorBeforeJump md cursorAfter buf =
    let
        isJump md =
            case md of
                V.BufferTop ->
                    True

                V.BufferBottom ->
                    True

                V.ViewTop ->
                    True

                V.ViewMiddle ->
                    True

                V.ViewBottom ->
                    True

                V.MatchString _ ->
                    True

                V.MatchPair ->
                    True

                _ ->
                    False
    in
        if buf.cursor /= cursorAfter && isJump md then
            let
                jumps =
                    saveJump
                        { path = buf.path
                        , cursor = buf.cursor
                        }
                        buf.jumps
            in
                { buf | jumps = jumps }
        else
            buf


motion :
    Maybe Int
    -> V.MotionData
    -> V.MotionOption
    -> Buffer
    -> ( Buffer, Cmd Msg )
motion count md mo buf =
    case runMotion count md mo buf of
        Just cursor ->
            ( buf
                |> saveCursorBeforeJump md cursor
                |> Buf.setCursor cursor (isSaveColumn md)
                |> setVisualEnd cursor
                |> saveMotion md mo buf
            , Cmd.none
            )

        Nothing ->
            ( saveMotion md mo buf buf, Cmd.none )
