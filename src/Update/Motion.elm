module Update.Motion
    exposing
        ( saveMotion
        , setVisualEnd
        , setVisualBegin
        , runMotion
        , motion
        , matchString
        , matchAllStrings
        , wordStringUnderCursor
        , wORDStringUnderCursor
        )

import Model exposing (..)
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Update.Buffer as Buf
import Internal.Position exposing (Position, excludeRight)
import String
import Internal.PositionClass exposing (..)
import Regex as Re exposing (regex)
import Internal.Jumps exposing (saveJump)
import Update.Message exposing (Msg(..))
import Internal.TextObject exposing (wordUnderCursor, wORDUnderCursor)
import Helper.Helper exposing (repeatfn, safeRegex)
import Internal.Brackets exposing (pairBracketAt, bracketsParser)
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
                                    wordStringUnderCursor
                                        oldbuf.config.wordChars
                                        oldbuf.lines
                                        oldbuf.cursor
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

                        maybeRe =
                            s
                                |> safeRegex
                                |> Maybe.map Re.caseInsensitive

                        findNext re cursor =
                            Maybe.map Tuple.first
                                (matchString forward1
                                    re
                                    cursor
                                    buf.lines
                                )
                    in
                        Maybe.andThen
                            (\re ->
                                repeatfn
                                    (Maybe.withDefault 1 count)
                                    (findNext re)
                                    buf.cursor
                            )
                            maybeRe

                _ ->
                    Nothing


matchRegion : Int -> Re.Match -> ( Position, Position )
matchRegion y m =
    ( ( y, m.index )
    , ( y, m.index + String.length m.match )
    )


matchStringForward :
    Re.Regex
    -> Position
    -> Position
    -> B.TextBuffer
    -> Maybe ( Position, Position )
matchStringForward re cursor (( y, x ) as start) lines =
    case B.getLine y lines of
        Just line ->
            case
                line
                    |> Re.find Re.All re
                    |> List.filter (\m -> ( y, m.index ) >= start)
            of
                m :: rest ->
                    let
                        (( b, e ) as region) =
                            matchRegion y m
                    in
                        if b <= cursor && cursor < e then
                            case rest of
                                m1 :: _ ->
                                    Just <| matchRegion y m1

                                _ ->
                                    matchStringForward re cursor ( y + 1, 0 ) lines
                        else
                            Just region

                _ ->
                    matchStringForward re cursor ( y + 1, 0 ) lines

        _ ->
            Nothing


matchStringBackward :
    Re.Regex
    -> Position
    -> Position
    -> B.TextBuffer
    -> Maybe ( Position, Position )
matchStringBackward re cursor end lines =
    let
        x =
            Tuple.second end

        y =
            if x == 0 then
                Tuple.first end - 1
            else
                Tuple.first end
    in
        case B.getLine y lines of
            Just line ->
                case
                    line
                        |> Re.find Re.All re
                        |> List.filter (\m -> ( y, m.index ) < end)
                        |> List.reverse
                of
                    m :: rest ->
                        let
                            (( b, e ) as region) =
                                matchRegion y m
                        in
                            if b <= cursor && cursor < e then
                                case rest of
                                    m1 :: _ ->
                                        Just <| matchRegion y m1

                                    _ ->
                                        matchStringBackward re cursor ( y, 0 ) lines
                            else
                                Just region

                    _ ->
                        matchStringBackward re cursor ( y, 0 ) lines

            _ ->
                Nothing


matchAllStrings :
    Re.Regex
    -> Int
    -> Int
    -> B.TextBuffer
    -> List ( Position, Position )
matchAllStrings re begin end lines =
    lines
        |> B.indexedMapLinesToList begin
            end
            (\i line ->
                line
                    |> Re.find Re.All re
                    |> List.map
                        (\m ->
                            excludeRight
                                ( ( i, m.index )
                                , ( i, m.index + String.length m.match )
                                )
                        )
            )
        |> List.concat


matchString :
    Bool
    -> Re.Regex
    -> Position
    -> B.TextBuffer
    -> Maybe ( Position, Position )
matchString forward re pos lines =
    case
        (if forward then
            matchStringForward re pos pos lines
         else
            matchStringBackward re pos pos lines
        )
    of
        Just res ->
            Just <| excludeRight res

        _ ->
            let
                ( y, _ ) =
                    pos

                n =
                    B.count lines
            in
                if forward then
                    matchStringForward re
                        ( 0, -1 )
                        ( 0, 0 )
                        (B.sliceLines 0 (y + 1) lines)
                        |> Maybe.map excludeRight
                    --|> Debug.log "search hit bottom"
                else
                    matchStringBackward
                        re
                        ( n - y, 0 )
                        ( n - y, 0 )
                        (B.sliceLines y n lines)
                        |> Maybe.map
                            (\rg ->
                                let
                                    ( ( y1, x1 ), ( y2, x2 ) ) =
                                        rg
                                in
                                    excludeRight ( ( y1 + y, x1 ), ( y2 + y, x2 ) )
                            )



--|> Debug.log "search hit top"


runMotion :
    Maybe Int
    -> V.MotionData
    -> V.MotionOption
    -> Buffer
    -> Maybe Position
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
                            Buf.cursorLineFirst buf.lines (n - 1)

                        _ ->
                            Buf.cursorLineFirst buf.lines 0

                V.BufferBottom ->
                    case count of
                        Just n ->
                            (n - 1)
                                |> max 0
                                |> min (B.count buf.lines - 2)
                                |> Buf.cursorLineFirst buf.lines

                        _ ->
                            Buf.cursorLineFirst buf.lines (B.count buf.lines - 2)

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
                    (buf.view.scrollTop + (Maybe.withDefault 1 count) - 1)
                        |> Basics.min (bottomLine buf)
                        |> Buf.cursorLineFirst buf.lines

                V.ViewMiddle ->
                    Buf.cursorLineFirst
                        buf.lines
                        (middleLine buf)

                V.ViewBottom ->
                    (bottomLine buf - (Maybe.withDefault 1 count) + 1)
                        |> Basics.max buf.view.scrollTop
                        |> Buf.cursorLineFirst buf.lines

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
                                repeatfn (Maybe.withDefault 1 count)
                                    findNext
                                    buf.cursor

                        _ ->
                            Nothing

                V.MatchString str ->
                    case str of
                        V.LastSavedString ->
                            gotoMatchedString count mo buf

                        V.WordUnderCursor ->
                            wordStringUnderCursor
                                buf.config.wordChars
                                buf.lines
                                buf.cursor
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
                                    |> Buf.cursorLineFirst buf.lines

                        _ ->
                            case buf.view.matchedCursor of
                                Just ( a, b ) ->
                                    if buf.cursor == a then
                                        Just b
                                    else if buf.cursor == b then
                                        Just a
                                    else
                                        Nothing

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
                                                    >> Maybe.map
                                                        (\dx ->
                                                            ( y, x + dx )
                                                        )
                                                )
                                            |> Maybe.andThen
                                                (pairBracketAt
                                                    0
                                                    (Array.length buf.syntax)
                                                    buf.lines
                                                    buf.syntax
                                                )

                V.Paragraph ->
                    let
                        findNext y =
                            let
                                y1 =
                                    findParagraph mo.forward y buf.lines
                            in
                                if y == y1 then
                                    Nothing
                                else
                                    Just y1
                    in
                        buf.cursor
                            |> Tuple.first
                            |> repeatfn (Maybe.withDefault 1 count) findNext
                            |> Maybe.andThen (Buf.cursorLineFirst buf.lines)

                V.WordStart ->
                    case buf.mode of
                        Insert { startCursor } ->
                            findPositionDeleteWordBack
                                count
                                md
                                mo
                                startCursor
                                buf

                        _ ->
                            findPositionDefault count md mo buf

                V.NextLineFirst ->
                    let
                        n =
                            Maybe.withDefault 1 count

                        y =
                            Tuple.first buf.cursor
                    in
                        Buf.cursorLineFirst buf.lines (y + n)

                _ ->
                    findPositionDefault count md mo buf


findPositionDeleteWordBack :
    Maybe Int
    -> V.MotionData
    -> V.MotionOption
    -> Position
    -> Buffer
    -> Maybe Position
findPositionDeleteWordBack count md mo startCursor buf =
    let
        { cursor, lines, config } =
            buf

        ( y, x ) =
            cursor
    in
        lines
            |> findPositionInBuffer md mo y x config.wordChars
            |> Maybe.map
                (\(( y1, _ ) as res) ->
                    if
                        (res < startCursor)
                            && (startCursor < cursor)
                    then
                        startCursor
                    else if y1 /= y then
                        if x > 0 then
                            ( y, 0 )
                        else
                            ( y1, B.getLineMaxColumn y1 lines )
                    else
                        res
                )


findPositionDefault :
    Maybe Int
    -> V.MotionData
    -> V.MotionOption
    -> Buffer
    -> Maybe Position
findPositionDefault count md mo buf =
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


findParagraph : Bool -> Int -> B.TextBuffer -> Int
findParagraph forward start lines =
    let
        isLineEmpty =
            (==) B.lineBreak

        delta =
            if forward then
                1
            else
                -1

        findParagraphHelper isLastLineEmpty y =
            case B.getLine y lines of
                Just line ->
                    let
                        isCurrentLineEmpty =
                            isLineEmpty line
                    in
                        if isCurrentLineEmpty && not isLastLineEmpty then
                            y
                        else
                            findParagraphHelper isCurrentLineEmpty (y + delta)

                _ ->
                    if forward then
                        max (B.count lines - 2) 0
                    else
                        0
    in
        findParagraphHelper True start


wordStringUnderCursor :
    String
    -> B.TextBuffer
    -> Position
    -> Maybe ( Position, String )
wordStringUnderCursor wordChars lines cursor =
    lines
        |> wordUnderCursor wordChars cursor
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

                V.Paragraph ->
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


showErrorMessage : V.MotionData -> V.MotionOption -> Buffer -> Buffer
showErrorMessage md mo buf =
    case md of
        V.RepeatMatchChar ->
            Buf.errorMessage "Char not found" buf

        V.MatchChar _ _ ->
            Buf.errorMessage "Char not found" buf

        V.MatchString _ ->
            Buf.errorMessage "Pattern not found" buf

        _ ->
            buf


showSuccessMessage : V.MotionData -> V.MotionOption -> Buffer -> Buffer
showSuccessMessage md mo buf =
    case md of
        V.RepeatMatchChar ->
            Buf.clearMessage buf

        V.MatchChar _ _ ->
            Buf.clearMessage buf

        V.MatchString _ ->
            case buf.last.matchString of
                Just ( s, _ ) ->
                    Buf.infoMessage
                        ((if mo.forward then
                            "/"
                          else
                            "?"
                         )
                            ++ s
                        )
                        buf

                _ ->
                    Buf.clearMessage buf

        _ ->
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
            let
                centerScrollTop =
                    Buf.bestScrollTop
                        (Tuple.first cursor)
                        buf.view.size.height
                        buf.lines
                        buf.view.scrollTop

                scrollTop =
                    case md of
                        V.MatchString _ ->
                            centerScrollTop

                        V.BufferTop ->
                            centerScrollTop

                        V.BufferBottom ->
                            centerScrollTop

                        _ ->
                            buf.view.scrollTop
            in
                ( buf
                    |> saveCursorBeforeJump md cursor
                    |> Buf.setCursor cursor (isSaveColumn md)
                    |> Buf.setScrollTop scrollTop
                    |> setVisualEnd cursor
                    |> saveMotion md mo buf
                    |> showSuccessMessage md mo
                , Cmd.none
                )

        Nothing ->
            ( buf
                |> saveMotion md mo buf
                |> showErrorMessage md mo
            , Cmd.none
            )
