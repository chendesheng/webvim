module Update exposing (update, init, initCommand, initMode)

import Http
import Internal.Brackets exposing (pairBracket, pairBracketAt)
import Char
import Task
import Update.Keymap exposing (keymap)
import Window as Win exposing (Size)
import Json.Decode as Decode
import Model exposing (..)
import Update.Message exposing (..)
import Vim.Helper exposing (parseKeys, escapeKey)
import Vim.AST exposing (AST)
import Helper.Helper
    exposing
        ( fromListBy
        , filename
        , safeRegex
        , isSpace
        , notSpace
        , resolvePath
        , relativePath
        , normalizePath
        , nthList
        )
import Vim.Parser exposing (parse)
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Update.Buffer as Buf
import Dict exposing (Dict)
import Parser as P exposing ((|.), (|=), Parser)
import Update.Motion exposing (..)
import Update.Delete exposing (..)
import Update.Insert exposing (..)
import Internal.PositionClass exposing (findLineFirst)
import Regex as Re
import Internal.TextObject exposing (expandTextObject)
import Result
import List
import Tuple
import String
import Update.Service exposing (..)
import Update.Yank exposing (yank)
import Helper.Debounce exposing (debounceLint, debounceTokenize)
import Elm.Array as Array exposing (Array)
import Helper.Document as Doc
import Internal.Jumps
    exposing
        ( saveJump
        , jumpForward
        , jumpBackward
        , Jumps
        , Location
        , currentLocation
        )
import Update.Range exposing (operatorRanges, shrinkRight)
import Update.AutoComplete exposing (..)
import Internal.Position exposing (Position)
import Update.CaseOperator exposing (applyCaseOperator)


stringToPrefix : String -> ExPrefix
stringToPrefix prefix =
    case prefix of
        "/" ->
            ExSearch { forward = True, match = Nothing }

        "?" ->
            ExSearch { forward = False, match = Nothing }

        "=" ->
            ExEval

        _ ->
            ExCommand


prefixToString : ExPrefix -> String
prefixToString prefix =
    case prefix of
        ExSearch { forward } ->
            if forward then
                "/"
            else
                "?"

        ExEval ->
            "="

        ExCommand ->
            ":"


initMode : Buffer -> V.ModeName -> Mode
initMode { cursor, mode } modeName =
    case modeName of
        V.ModeNameNormal ->
            case mode of
                Ex { message } ->
                    Normal { message = message }

                _ ->
                    Normal { message = EmptyMessage }

        V.ModeNameTempNormal ->
            TempNormal

        V.ModeNameInsert ->
            Insert
                { autoComplete = Nothing
                , startCursor = cursor
                }

        V.ModeNameEx prefix ->
            Ex
                { prefix = (stringToPrefix prefix)
                , exbuf =
                    emptyExBuffer
                        |> Buf.transaction
                            [ Insertion ( 0, 0 ) <|
                                B.fromString prefix
                            ]
                , visual =
                    case mode of
                        Visual visual ->
                            Just visual

                        _ ->
                            Nothing
                , message = EmptyMessage
                }

        V.ModeNameVisual tipe ->
            Visual
                (case mode of
                    Visual visual ->
                        { visual | tipe = tipe }

                    Ex { visual } ->
                        case visual of
                            Just v ->
                                v

                            _ ->
                                { tipe = tipe
                                , begin = cursor
                                , end = cursor
                                }

                    _ ->
                        { tipe = tipe
                        , begin = cursor
                        , end = cursor
                        }
                )


getModeName : Mode -> V.ModeName
getModeName mode =
    case mode of
        Normal _ ->
            V.ModeNameNormal

        Insert _ ->
            V.ModeNameInsert

        TempNormal ->
            V.ModeNameTempNormal

        Visual { tipe } ->
            V.ModeNameVisual tipe

        Ex { prefix } ->
            V.ModeNameEx <| prefixToString prefix


updateMode : V.ModeName -> Buffer -> Buffer
updateMode modeName buf =
    let
        oldModeName =
            getModeName buf.mode

        last =
            buf.last

        newMode =
            if oldModeName == modeName then
                buf.mode
            else if
                (oldModeName == V.ModeNameNormal)
                    && (modeName == V.ModeNameInsert)
                    && buf.last.motionFailed
            then
                -- Don't change to insert mode when motion failed
                buf.mode
            else
                initMode buf modeName

        buf1 =
            { buf | mode = newMode }

        view =
            buf1.view

        scrollFrom =
            Buf.finalScrollTop buf

        scrollTo =
            Buf.finalScrollTop buf1
    in
        { buf1
            | last = { last | motionFailed = False }
            , view =
                { view
                    | lines =
                        Buf.scrollViewLines
                            buf1.lines
                            buf1.syntax
                            view.size.height
                            scrollFrom
                            scrollTo
                            view.lines
                }
        }


isLineDeltaMotion : Operator -> Bool
isLineDeltaMotion op =
    case op of
        Move mo _ ->
            case mo of
                V.LineDelta ->
                    True

                _ ->
                    False

        _ ->
            False


modeChanged : Bool -> Key -> V.ModeName -> Bool -> Buffer -> Buffer
modeChanged replaying key oldModeName lineDeltaMotion buf =
    case buf.mode of
        Normal _ ->
            let
                ( y, x ) =
                    buf.cursor

                cursor =
                    if oldModeName == V.ModeNameInsert then
                        ( y, max (x - 1) 0 )
                    else
                        ( y
                        , if B.getLineMaxColumn y buf.lines > x then
                            x
                          else
                            max (x - 1) 0
                        )

                changeColumn =
                    if lineDeltaMotion then
                        False
                    else
                        cursor /= buf.cursor
            in
                buf
                    |> Buf.setCursor
                        cursor
                        changeColumn
                    |> Buf.cancelLastIndent
                    |> Buf.commit

        TempNormal ->
            buf
                |> Buf.commit

        Ex ({ prefix, exbuf } as ex) ->
            if B.isEmpty exbuf.lines then
                buf
                    |> handleKeypress False "<esc>"
                    |> Tuple.first
            else
                let
                    last =
                        buf.last

                    prefix1 =
                        case prefix of
                            ExSearch ({ forward } as search) ->
                                case
                                    exbuf.lines
                                        |> B.toString
                                        |> String.dropLeft 1
                                        |> safeRegex
                                        |> Maybe.map Re.caseInsensitive
                                of
                                    Just re ->
                                        ExSearch
                                            { search
                                                | match =
                                                    matchString forward
                                                        re
                                                        buf.cursor
                                                        buf.lines
                                            }

                                    _ ->
                                        ExSearch
                                            { search | match = Nothing }

                            _ ->
                                prefix

                    buf1 =
                        if replaying || key == "<exbuf>" then
                            buf
                        else
                            case oldModeName of
                                V.ModeNameEx _ ->
                                    { buf
                                        | last = { last | ex = last.ex ++ key }
                                    }

                                _ ->
                                    { buf | last = { last | ex = "" } }

                    buf2 =
                        Buf.setMode
                            (Ex { ex | prefix = prefix1 })
                            buf1

                    scrollFrom =
                        Buf.finalScrollTop buf1

                    scrollTo =
                        Buf.finalScrollTop buf2
                in
                    buf2
                        |> Buf.updateView
                            (\view ->
                                { view
                                    | lines =
                                        Buf.scrollViewLines
                                            buf2.lines
                                            buf2.syntax
                                            view.size.height
                                            scrollFrom
                                            scrollTo
                                            view.lines
                                }
                            )

        Insert _ ->
            let
                last =
                    buf.last

                inserts =
                    if replaying || key == "<inserts>" then
                        last.inserts
                    else
                        case oldModeName of
                            V.ModeNameInsert ->
                                last.inserts ++ key

                            _ ->
                                ""
            in
                { buf | last = { last | inserts = inserts } }

        Visual _ ->
            let
                last =
                    buf.last

                visual =
                    if replaying || key == "<visual>" then
                        last.visual
                    else
                        case oldModeName of
                            V.ModeNameVisual _ ->
                                last.visual ++ key

                            _ ->
                                ""
            in
                { buf | last = { last | visual = visual } }


setContinuation : String -> Buffer -> Buffer
setContinuation s buf =
    { buf | continuation = s }


floorFromZero : Float -> Int
floorFromZero n =
    if n < 0 then
        ceiling n
    else
        floor n


expandVisual :
    comparable
    -> comparable
    -> comparable
    -> comparable
    -> ( comparable, comparable )
expandVisual begin end a b =
    if begin < end then
        ( (min a begin), (max b end) )
    else
        ( (max b begin), (min a end) )


cmdNone : Buffer -> ( Buffer, Cmd Msg )
cmdNone buf =
    ( buf, Cmd.none )


correctLines : Buffer -> Buffer
correctLines buf =
    if B.isEmpty buf.lines then
        Buf.transaction
            [ Insertion buf.cursor <|
                B.fromString B.lineBreak
            ]
            buf
    else
        buf


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


clearExBufAutoComplete : Buffer -> Buffer
clearExBufAutoComplete exbuf =
    { exbuf
        | mode =
            case exbuf.mode of
                Insert insert ->
                    Insert
                        { insert
                            | autoComplete =
                                Nothing
                        }

                _ ->
                    initMode exbuf
                        V.ModeNameInsert
    }


replaceChar : String -> Buffer -> Buffer
replaceChar ch buf =
    let
        ( y, x ) =
            buf.cursor
    in
        buf
            |> Buf.transaction
                [ Deletion buf.cursor ( y, x + 1 ) ]
            |> insert (V.TextLiteral ch)


replaceRegion : String -> Position -> Position -> Buffer -> Buffer
replaceRegion ch b e buf =
    let
        s =
            buf.lines
                |> B.sliceRegion b e
                |> B.toString
                |> Re.replace Re.All (Re.regex "[^\x0D\n]") (always ch)
                |> B.fromString
    in
        buf
            |> Buf.setCursor b False
            |> Buf.transaction
                [ Deletion b e
                , Insertion b s
                ]
            |> Buf.setCursor ( Tuple.first b, Tuple.second b + 1 ) True


runOperator : Maybe Int -> String -> Operator -> Buffer -> ( Buffer, Cmd Msg )
runOperator count register operator buf =
    case operator of
        Move md mo ->
            motion count md mo buf

        Select textobj around ->
            case buf.mode of
                Visual { tipe, begin, end } ->
                    case tipe of
                        V.VisualChars ->
                            (if begin == end then
                                begin
                             else if buf.cursor == max begin end then
                                Tuple.mapSecond ((+) 1) (max begin end)
                             else
                                Tuple.mapSecond (flip (-) 1) (min begin end)
                            )
                                |> expandTextObject buf.config.wordChars
                                    buf.view.scrollTop
                                    buf.view.size.height
                                    buf.syntax
                                    textobj
                                    around
                                    buf.lines
                                |> Maybe.map
                                    (\rg ->
                                        let
                                            ( a, b ) =
                                                shrinkRight rg

                                            begin1 =
                                                if begin == end then
                                                    min a b
                                                else
                                                    a
                                                        |> min b
                                                        |> min begin
                                                        |> min end

                                            end1 =
                                                if begin == end then
                                                    max a b
                                                else
                                                    a
                                                        |> max b
                                                        |> max begin
                                                        |> max end
                                        in
                                            if
                                                (buf.cursor == min begin end)
                                                    && (begin /= end)
                                            then
                                                buf
                                                    |> Buf.setCursor begin1 True
                                                    |> setVisualEnd begin1
                                                    |> setVisualBegin end1
                                            else
                                                buf
                                                    |> Buf.setCursor end1 True
                                                    |> setVisualEnd end1
                                                    |> setVisualBegin begin1
                                    )
                                |> Maybe.withDefault buf
                                |> cmdNone

                        _ ->
                            ( buf, Cmd.none )

                _ ->
                    ( buf, Cmd.none )

        Scroll value ->
            -- TODO: scroll in visual mode
            let
                scope n =
                    n
                        |> max 0
                        |> min (B.count buf.lines - 1)

                setCursor buf =
                    case count of
                        Just n ->
                            let
                                y =
                                    scope (n - 1)

                                x =
                                    Tuple.second buf.cursor
                            in
                                case value of
                                    V.ScrollToTop ->
                                        Buf.setCursor ( y, x ) False buf

                                    V.ScrollToBottom ->
                                        Buf.setCursor ( y, x ) False buf

                                    V.ScrollToMiddle ->
                                        Buf.setCursor ( y, x ) False buf

                                    _ ->
                                        buf

                        _ ->
                            buf

                scroll buf =
                    let
                        y =
                            Tuple.first buf.cursor

                        y1 =
                            case value of
                                V.ScrollBy n ->
                                    count
                                        |> Maybe.withDefault 1
                                        |> ((*) n)
                                        |> ((+) buf.view.scrollTop)

                                V.ScrollToTop ->
                                    y

                                V.ScrollToBottom ->
                                    y - buf.view.size.height - 2 + 1

                                V.ScrollToMiddle ->
                                    y - (buf.view.size.height - 2) // 2
                    in
                        Buf.setScrollTop (scope y1) buf
            in
                buf
                    |> setCursor
                    |> scroll
                    |> cursorScope
                    |> cmdNone

        InsertString s ->
            case s of
                V.LastSavedString ->
                    replayKeys buf.last.inserts buf

                _ ->
                    buf
                        |> insert s
                        |> filterAutoComplete
                        |> cmdNone

        Delete rg ->
            buf
                |> delete count register rg
                |> filterAutoComplete
                |> cmdNone

        Yank rg ->
            yank count register rg buf

        Undo ->
            buf
                |> Buf.undo
                |> cmdNone

        Redo ->
            buf
                |> Buf.redo
                |> Buf.indentCursorToLineFirst
                |> cmdNone

        OpenNewLine forward ->
            buf
                |> openNewLine
                    (if forward then
                        Tuple.first buf.cursor + 1
                     else
                        Tuple.first buf.cursor
                    )
                |> cmdNone

        JumpByView factor ->
            let
                forward =
                    factor > 0

                view =
                    buf.view

                height =
                    view.size.height

                lineScope row =
                    row
                        |> max 0
                        |> min (B.count buf.lines - 1)

                scrollScope scrollTop n =
                    let
                        newn =
                            scrollTop + n

                        maxy =
                            B.count buf.lines - 1
                    in
                        if forward then
                            if (newn + height) > maxy then
                                max (maxy - height) 0
                            else
                                newn
                        else
                            (if newn < height then
                                0
                             else
                                newn
                            )

                n =
                    floorFromZero (toFloat height * factor)

                y =
                    lineScope (Tuple.first buf.cursor + n)

                scrollTop =
                    scrollScope view.scrollTop n
            in
                case Buf.cursorLineFirst buf.lines y of
                    Just cursor ->
                        buf
                            |> Buf.setCursor cursor True
                            |> setVisualEnd cursor
                            |> Buf.setScrollTop scrollTop
                            |> cmdNone

                    Nothing ->
                        ( buf, Cmd.none )

        Put forward ->
            let
                removeRegister reg buf =
                    { buf | registers = Dict.remove reg buf.registers }
            in
                Dict.get register buf.registers
                    |> Maybe.map
                        (\s ->
                            case buf.mode of
                                Ex ({ exbuf } as ex) ->
                                    buf
                                        |> Buf.setMode
                                            (Ex
                                                { ex
                                                    | exbuf =
                                                        Buf.putString
                                                            forward
                                                            s
                                                            exbuf
                                                }
                                            )
                                        |> removeRegister "+"

                                _ ->
                                    buf
                                        |> Buf.putString forward s
                                        |> removeRegister "+"
                        )
                    |> Maybe.withDefault buf
                    |> cmdNone

        RepeatLastOperator ->
            replayKeys buf.dotRegister buf

        RepeatLastVisual ->
            replayKeys buf.last.visual buf

        RepeatLastEx ->
            replayKeys buf.last.ex buf

        VisualSwitchEnd ->
            case buf.mode of
                Visual { tipe, begin, end } ->
                    buf
                        |> Buf.setMode
                            (Visual
                                { tipe = tipe
                                , begin = end
                                , end = begin
                                }
                            )
                        |> Buf.setCursor begin True
                        |> cmdNone

                _ ->
                    ( buf, Cmd.none )

        Execute ->
            case buf.mode of
                Ex ex ->
                    let
                        exbuf =
                            ex.exbuf

                        exbuf1 =
                            clearExBufAutoComplete exbuf
                    in
                        ex.exbuf.lines
                            |> B.toString
                            |> String.dropLeft 1
                            |> flip (execute count register)
                                { buf | mode = Ex { ex | exbuf = exbuf1 } }

                _ ->
                    ( buf, Cmd.none )

        Join collapseSpaces ->
            buf
                |> join count collapseSpaces
                |> cmdNone

        Replace ch ->
            case buf.mode of
                Normal _ ->
                    buf
                        |> replaceChar ch
                        |> cmdNone

                TempNormal ->
                    buf
                        |> replaceChar ch
                        |> (\buf ->
                                let
                                    ( y, x ) =
                                        buf.cursor
                                in
                                    Buf.setCursor ( y, max 0 (x - 1) ) True buf
                           )
                        |> cmdNone

                Visual _ ->
                    let
                        regions =
                            operatorRanges count (V.VisualRange False) buf
                                |> List.reverse
                    in
                        ( List.foldl
                            (\( b, e ) buf -> replaceRegion ch b e buf)
                            buf
                            regions
                        , Cmd.none
                        )

                _ ->
                    ( buf, Cmd.none )

        ToggleTip ->
            buf
                |> Buf.setShowTip (not buf.view.showTip)
                |> cmdNone

        SelectAutoComplete forward ->
            case buf.mode of
                Ex ex ->
                    ( ex.exbuf
                        |> selectAutoComplete forward
                        |> setExbuf buf ex
                    , Cmd.none
                    )

                Insert { autoComplete } ->
                    case autoComplete of
                        Just _ ->
                            ( selectAutoComplete forward buf
                            , Cmd.none
                            )

                        _ ->
                            ( case autoCompleteTarget buf.config.wordChars buf of
                                Just ( pos, word ) ->
                                    buf
                                        |> startAutoComplete
                                            buf.config.wordChars
                                            (Buf.toWords word buf)
                                            pos
                                            word
                                        |> selectAutoComplete forward

                                _ ->
                                    buf
                            , Cmd.none
                            )

                _ ->
                    ( buf, Cmd.none )

        JumpHistory isForward ->
            let
                jumps =
                    if isForward then
                        jumpForward buf.jumps
                    else
                        jumpBackward
                            { path = buf.path
                            , cursor = buf.cursor
                            }
                            buf.jumps
            in
                case currentLocation jumps of
                    Just loc ->
                        jumpToLocation False
                            loc
                            { buf | jumps = jumps }

                    _ ->
                        ( buf, Cmd.none )

        JumpLastBuffer ->
            case Dict.get "#" buf.registers of
                Just reg ->
                    jumpToPath True (registerString reg) Nothing buf

                _ ->
                    ( buf, Cmd.none )

        JumpToTag ->
            case
                wordStringUnderCursor
                    buf.config.wordChars
                    buf.lines
                    buf.cursor
            of
                Just ( _, s ) ->
                    ( buf
                    , sendReadTags buf.config.service
                        buf.config.pathSeperator
                        buf.cwd
                        buf.path
                        (Maybe.withDefault 1 count - 1)
                        s
                    )

                _ ->
                    ( buf, Cmd.none )

        JumpBackFromTag ->
            case buf.last.jumpToTag of
                Just loc ->
                    jumpToLocation True loc buf

                _ ->
                    ( buf, Cmd.none )

        JumpToFile ->
            case wORDStringUnderCursor buf of
                Just ( _, s ) ->
                    let
                        locationParser =
                            P.succeed
                                (\path ints ->
                                    ( path
                                    , case ints of
                                        y :: x :: _ ->
                                            ( y - 1, x - 1 )

                                        [ y ] ->
                                            ( y - 1, 0 )

                                        _ ->
                                            ( 0, 0 )
                                    )
                                )
                                |= P.keep P.oneOrMore (\c -> notSpace c && (c /= ':'))
                                |= ((P.succeed identity
                                        |. P.symbol ":"
                                        |= P.oneOf
                                            [ P.int
                                            , P.succeed 1
                                            ]
                                    )
                                        |> P.repeat P.zeroOrMore
                                   )
                    in
                        case P.run locationParser s of
                            Ok ( path, cursor ) ->
                                jumpToPath True path (Just cursor) buf

                            _ ->
                                ( buf, Cmd.none )

                _ ->
                    ( buf, Cmd.none )

        Indent forward range ->
            let
                genPatches y =
                    if forward then
                        buf.lines
                            |> B.getLine y
                            |> Maybe.map
                                (\s ->
                                    " "
                                        |> String.repeat
                                            (if String.length s > 1 then
                                                buf.config.tabSize
                                             else
                                                0
                                            )
                                        |> B.fromString
                                        |> Insertion ( y, 0 )
                                )
                            |> Maybe.withDefault (Insertion ( y, 0 ) B.empty)
                    else
                        let
                            size =
                                buf.lines
                                    |> B.getLine y
                                    |> Maybe.map findLineFirst
                                    |> Maybe.withDefault 0
                                    |> min
                                        buf.config.tabSize
                        in
                            Deletion
                                ( y, 0 )
                                ( y, size )

                lineNumbers =
                    buf
                        |> operatorRanges count range
                        |> List.concatMap
                            (\rg ->
                                let
                                    ( begin, end ) =
                                        rg

                                    ( y, x ) =
                                        end
                                in
                                    List.range
                                        (Tuple.first begin)
                                        (y
                                            |> ((+)
                                                    (if x == 0 then
                                                        -1
                                                     else
                                                        0
                                                    )
                                               )
                                            |> max 0
                                        )
                            )
            in
                case lineNumbers of
                    y :: _ ->
                        let
                            buf1 =
                                Buf.transaction
                                    (List.map genPatches lineNumbers)
                                    buf
                        in
                            ( Buf.gotoLine y buf1, Cmd.none )

                    _ ->
                        ( buf, Cmd.none )

        IncreaseNumber larger ->
            let
                ( y, x ) =
                    buf.cursor

                delta =
                    if larger then
                        Maybe.withDefault 1 count
                    else
                        -(Maybe.withDefault 1 count)

                numParser =
                    P.succeed
                        (\pre s ->
                            ( String.length pre
                            , String.length s
                            , s
                                |> String.toInt
                                |> Result.withDefault 0
                            )
                        )
                        |= P.keep P.zeroOrMore (Char.isDigit >> not)
                        |= P.keep P.oneOrMore Char.isDigit
            in
                buf.lines
                    |> B.getLine y
                    |> Maybe.andThen
                        (\line ->
                            line
                                |> String.slice x -1
                                |> P.run numParser
                                |> Result.toMaybe
                                |> Maybe.map
                                    (\res ->
                                        let
                                            ( dx, len, n ) =
                                                res

                                            isNegative =
                                                String.slice
                                                    (x + dx - 1)
                                                    (x + dx)
                                                    line
                                                    == "-"

                                            dx1 =
                                                if isNegative then
                                                    dx - 1
                                                else
                                                    dx

                                            n1 =
                                                if isNegative then
                                                    -n
                                                else
                                                    n

                                            len1 =
                                                if isNegative then
                                                    len + 1
                                                else
                                                    len

                                            cursor =
                                                ( y, x + dx1 )

                                            patches =
                                                [ Deletion cursor
                                                    ( y
                                                    , x + dx1 + len1
                                                    )
                                                , n1
                                                    |> ((+) delta)
                                                    |> toString
                                                    |> B.fromString
                                                    |> Insertion cursor
                                                ]
                                        in
                                            buf
                                                |> Buf.transaction
                                                    patches
                                                |> Buf.setCursor
                                                    cursor
                                                    True
                                    )
                        )
                    |> Maybe.withDefault buf
                    |> cmdNone

        CaseOperator changeCase range ->
            ( applyCaseOperator count changeCase range buf, Cmd.none )

        _ ->
            ( buf, Cmd.none )


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


newBuffer : BufferInfo -> Buffer -> Buffer
newBuffer info buf =
    let
        { cursor, path, version, content } =
            info

        ( name, ext ) =
            filename path

        config =
            Buf.configs
                |> Dict.get ext
                |> Maybe.withDefault defaultBufferConfig

        ( lines, syntax ) =
            Maybe.withDefault ( emptyBuffer.lines, emptyBuffer.syntax ) content

        height =
            buf.view.size.height

        scrollTop =
            Buf.bestScrollTop (Tuple.first cursor)
                height
                lines
                0
    in
        { buf
            | lines = lines
            , mode = Normal { message = EmptyMessage }
            , config =
                { config
                    | service = buf.config.service
                    , pathSeperator = buf.config.pathSeperator
                    , syntax = info.syntax
                }
            , view =
                { emptyView
                    | size = buf.view.size
                    , lineHeight = buf.view.lineHeight
                    , scrollTopPx = scrollTop * buf.view.lineHeight
                    , scrollTop = scrollTop
                    , lines =
                        Buf.getViewLines
                            scrollTop
                            (scrollTop + height + 2)
                            lines
                            syntax
                            |> Buf.fillEmptyViewLines height
                }
            , cursor = cursor
            , lint = { items = [], count = 0 }
            , cursorColumn = Tuple.second cursor
            , path = path
            , name = name ++ ext
            , history = { emptyBufferHistory | version = version }
            , syntax = syntax
            , syntaxDirtyFrom = Array.length syntax
        }
            |> correctCursor
            |> scrollToCursor


isExEditing : Operator -> Bool
isExEditing op =
    case op of
        Delete _ ->
            True

        InsertString _ ->
            True

        _ ->
            False


triggerExAutoComplete : Buffer -> Bool
triggerExAutoComplete buf =
    buf.lines
        |> B.toString
        |> String.startsWith ":e "


setExbuf : Buffer -> ExMode -> Buffer -> Buffer
setExbuf buf ex exbuf =
    { buf | mode = Ex { ex | exbuf = exbuf } }


applyEdit : Maybe Int -> Maybe Operator -> String -> Buffer -> ( Buffer, Cmd Msg )
applyEdit count edit register buf =
    case edit of
        Just operator ->
            case buf.mode of
                Ex ex ->
                    let
                        ( buf1, cmd ) =
                            buf
                                |> runOperator count register operator
                    in
                        case buf1.mode of
                            Ex ({ exbuf } as newex) ->
                                if triggerExAutoComplete exbuf then
                                    if isExEditing operator then
                                        if isAutoCompleteStarted exbuf then
                                            ( exbuf
                                                |> filterAutoComplete
                                                |> setExbuf buf1 newex
                                            , cmd
                                            )
                                        else
                                            ( buf1
                                            , Cmd.batch
                                                [ cmd
                                                , sendListFiles
                                                    buf.config.service
                                                    buf.config.pathSeperator
                                                    buf.cwd
                                                ]
                                            )
                                    else
                                        ( buf1, cmd )
                                else
                                    ( setExbuf buf1
                                        newex
                                        (clearExBufAutoComplete exbuf)
                                    , cmd
                                    )

                            _ ->
                                ( buf1, cmd )

                _ ->
                    buf
                        |> runOperator count register operator

        Nothing ->
            ( buf, Cmd.none )


isEnterInsertMode : Maybe Operator -> Bool
isEnterInsertMode edit =
    case edit of
        Just operator ->
            case operator of
                Put _ ->
                    True

                Replace _ ->
                    True

                _ ->
                    False

        _ ->
            False


replayKeys : String -> Buffer -> ( Buffer, Cmd Msg )
replayKeys s buf =
    if s == "" then
        ( buf, Cmd.none )
    else
        let
            savedLast =
                buf.last

            savedRegisters =
                buf.registers

            keys =
                s
                    |> parseKeys
                    |> Maybe.withDefault []

            ( buf1, cmd ) =
                List.foldl
                    (\key ( buf, cmd ) ->
                        let
                            ( buf1, cmd1 ) =
                                handleKeypress True key buf
                        in
                            ( buf1, Cmd.batch [ cmd, cmd1 ] )
                    )
                    ( buf, Cmd.none )
                    keys
        in
            ( { buf1
                | last = savedLast
                , registers = savedRegisters
              }
            , cmd
            )


isModeNameVisual : V.ModeName -> Bool
isModeNameVisual name =
    case name of
        V.ModeNameVisual _ ->
            True

        _ ->
            False


tokenizeBuffer : Buffer -> ( Buffer, Cmd Msg )
tokenizeBuffer buf =
    ( buf, tokenizeBufferCmd buf )


isTempBuffer : String -> Bool
isTempBuffer path =
    String.isEmpty path || path == "[Search]"


tokenizeBufferCmd : Buffer -> Cmd Msg
tokenizeBufferCmd buf =
    if isTempBuffer buf.path || not buf.config.syntax then
        Cmd.none
    else
        let
            begin =
                buf.syntaxDirtyFrom

            end =
                Buf.finalScrollTop buf + 2 * buf.view.size.height

            lines =
                if begin < end then
                    buf.lines
                        |> B.sliceLines begin end
                        |> B.toString
                else
                    ""
        in
            if String.isEmpty lines then
                Cmd.none
            else
                sendTokenize
                    buf.config.service
                    { path = buf.path
                    , version = buf.history.version
                    , line = begin
                    , lines =
                        buf.lines
                            |> B.sliceLines begin end
                            |> B.toString
                    }


jumpTo : Bool -> BufferInfo -> Buffer -> ( Buffer, Cmd Msg )
jumpTo isSaveJump info buf =
    let
        { path, cursor } =
            info

        jumps =
            if isSaveJump then
                saveJump { path = buf.path, cursor = buf.cursor } buf.jumps
            else
                buf.jumps
    in
        if path == buf.path then
            { buf | jumps = jumps }
                |> Buf.setCursor cursor True
                |> Buf.setScrollTop
                    (Buf.bestScrollTop (Tuple.first cursor)
                        buf.view.size.height
                        buf.lines
                        buf.view.scrollTop
                    )
                |> tokenizeBuffer
        else
            editBuffer
                info
                { buf | jumps = jumps }


jumpToLocation : Bool -> Location -> Buffer -> ( Buffer, Cmd Msg )
jumpToLocation isSaveJump { path, cursor } buf =
    jumpToPath isSaveJump path (Just cursor) buf


jumpToPath : Bool -> String -> Maybe Position -> Buffer -> ( Buffer, Cmd Msg )
jumpToPath isSaveJump path_ overrideCursor buf =
    let
        path =
            if isTempBuffer path_ then
                path_
            else
                resolvePath
                    buf.config.pathSeperator
                    buf.cwd
                    path_

        info =
            buf.buffers
                |> Dict.get path
                |> Maybe.map
                    (\info ->
                        { info
                            | cursor =
                                Maybe.withDefault info.cursor overrideCursor
                        }
                    )
                |> Maybe.withDefault
                    { emptyBufferInfo
                        | path = path
                        , cursor = Maybe.withDefault ( 0, 0 ) overrideCursor
                    }
    in
        jumpTo isSaveJump info buf


editTestBuffer : String -> Buffer -> ( Buffer, Cmd Msg )
editTestBuffer path buf =
    Ok
        { emptyBufferInfo
            | content =
                Just
                    ( B.fromString B.lineBreak
                    , Array.empty
                    )
            , path = path
        }
        |> Read
        |> flip update buf


execute : Maybe Int -> String -> String -> Buffer -> ( Buffer, Cmd Msg )
execute count register s buf =
    case
        s
            |> String.trim
            |> String.split " "
    of
        [ "e", path ] ->
            -- for unit testing
            if path == "*Test*" then
                editTestBuffer path buf
            else
                jumpToPath
                    True
                    (normalizePath buf.config.pathSeperator path)
                    Nothing
                    buf

        [ "w" ] ->
            ( buf, sendWriteBuffer buf.config.service buf.path buf )

        [ "ll" ] ->
            let
                n =
                    (Maybe.withDefault 1 count) - 1
            in
                case nthList n buf.locationList of
                    Just loc ->
                        jumpToLocation True loc buf

                    _ ->
                        ( Buf.errorMessage "location not found" buf
                        , Cmd.none
                        )

        [ "f", s ] ->
            ( buf, sendSearch buf.config.service buf.cwd s )

        [ "cd" ] ->
            ( Buf.infoMessage buf.cwd buf, Cmd.none )

        [ "cd", cwd ] ->
            ( buf, sendCd buf.config.service cwd )

        [ s ] ->
            case String.toInt s of
                Ok n ->
                    runOperator (Just n)
                        register
                        (V.Move V.BufferBottom V.emptyMotionOption)
                        buf

                _ ->
                    ( buf, Cmd.none )

        _ ->
            ( buf, Cmd.none )


handleKeypress : Bool -> Key -> Buffer -> ( Buffer, Cmd Msg )
handleKeypress replaying key buf =
    let
        cacheKey =
            ( buf.continuation, key )

        (( ast, continuation ) as cacheVal) =
            case Dict.get cacheKey buf.vimASTCache of
                Just resp ->
                    resp

                _ ->
                    parse buf.continuation key

        buf1 =
            buf
                |> cacheVimAST cacheKey cacheVal
                |> setContinuation continuation
    in
        case serviceBeforeApplyVimAST replaying key ast buf1 of
            Just cmd ->
                ( buf1, cmd )

            _ ->
                applyVimAST replaying key ast buf1


serviceBeforeApplyVimAST : Bool -> Key -> AST -> Buffer -> Maybe (Cmd Msg)
serviceBeforeApplyVimAST replaying key ast buf =
    case ast.edit of
        Just op ->
            case op of
                Put forward ->
                    if ast.register == "+" then
                        sendReadClipboard
                            replaying
                            key
                            buf.config.service
                            ast
                            |> Just
                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


applyVimAST : Bool -> Key -> AST -> Buffer -> ( Buffer, Cmd Msg )
applyVimAST replaying key ast buf =
    let
        { count, edit, modeName, register, recordKeys } =
            ast

        -- |> Debug.log key
        oldModeName =
            -- For now the put operator is implemented as
            --   1) Start insert mode
            --   2) Put string
            --   3) Back to normal mode
            if modeName == V.ModeNameNormal && isEnterInsertMode edit then
                V.ModeNameInsert
            else
                getModeName buf.mode

        saveDotRegister replaying buf =
            if replaying then
                buf
            else
                case recordKeys of
                    "" ->
                        buf

                    s ->
                        { buf | dotRegister = s }

        lineDeltaMotion =
            edit
                |> Maybe.map isLineDeltaMotion
                |> Maybe.withDefault False

        setMatchedCursor oldBuf buf =
            if
                (pairSource oldBuf /= pairSource buf)
                    || (oldBuf.view.scrollTop /= buf.view.scrollTop)
                    || (oldBuf.view.size /= buf.view.size)
                    || (oldBuf.lines /= buf.lines)
                    || (oldBuf.syntax /= buf.syntax)
            then
                pairCursor buf
            else
                buf

        doTokenize oldBuf ( buf, cmds ) =
            if (oldBuf.path == buf.path) || (Buf.isEditing oldBuf buf) then
                ( buf, (debounceTokenize 100) :: cmds )
            else
                ( buf, cmds )

        doSetTitle oldBuf ( buf, cmds ) =
            if Buf.isDirty oldBuf /= Buf.isDirty buf then
                let
                    prefix =
                        if Buf.isDirty buf then
                            "• "
                        else
                            ""
                in
                    ( buf, Doc.setTitle (prefix ++ buf.name) :: cmds )
            else
                ( buf, cmds )

        doLint oldBuf ( buf, cmds ) =
            if Buf.isEditing oldBuf buf then
                let
                    delay =
                        case buf.mode of
                            Insert _ ->
                                500

                            _ ->
                                0
                in
                    ( buf, debounceLint delay :: cmds )
            else
                ( buf, cmds )
    in
        buf
            |> applyEdit count edit register
            |> Tuple.mapFirst
                (updateMode modeName
                    >> correctLines
                    >> modeChanged replaying key oldModeName lineDeltaMotion
                    >> correctCursor
                    >> scrollToCursor
                    >> saveDotRegister replaying
                    >> setMatchedCursor buf
                )
            |> Tuple.mapSecond List.singleton
            |> doLint buf
            |> doSetTitle buf
            |> doTokenize buf
            |> Tuple.mapSecond Cmd.batch


onTokenized :
    Buffer
    -> Result error TokenizeResponse
    -> ( Buffer, Cmd Msg )
onTokenized buf resp =
    case resp of
        Ok payload ->
            case payload of
                TokenizeSuccess begin syntax ->
                    ( let
                        syntax1 =
                            buf.syntax
                                |> Array.slice 0 begin
                                |> flip Array.append syntax

                        view =
                            buf.view

                        updateViewLineSyntax ({ lineNumber } as viewLine) =
                            if
                                (begin <= lineNumber)
                                    && (lineNumber < Array.length syntax1)
                            then
                                case Array.get lineNumber syntax1 of
                                    Just tokens ->
                                        { viewLine | syntax = tokens }

                                    _ ->
                                        viewLine
                            else
                                viewLine
                      in
                        { buf
                            | syntax = syntax1
                            , syntaxDirtyFrom = Array.length syntax1
                            , view =
                                { view
                                    | lines =
                                        view.lines
                                            |> List.map
                                                (Maybe.map updateViewLineSyntax)
                                }
                        }
                            |> pairCursor
                    , Cmd.none
                    )

                LineTokenizeSuccess begin tokens ->
                    tokenizeBuffer
                        { buf
                            | syntax = Array.set begin tokens buf.syntax
                            , syntaxDirtyFrom = begin + 1
                        }

                TokenizeCacheMiss ->
                    tokenizeBuffer
                        { buf
                            | syntax = Array.empty
                            , syntaxDirtyFrom = 0
                        }

                TokenizeError s ->
                    let
                        config =
                            buf.config
                    in
                        ( { buf
                            | syntax = Array.empty
                            , syntaxDirtyFrom = 0
                            , config = { config | syntax = False }
                          }
                        , Cmd.none
                        )

        Err _ ->
            ( buf, Cmd.none )


lintErrorToLocationList : List LintError -> List Location
lintErrorToLocationList items =
    List.map
        (\item ->
            { path = item.file
            , cursor =
                item.subRegion
                    |> Maybe.withDefault item.region
                    |> Tuple.first
            }
        )
        items


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


shortPath : Buffer -> String
shortPath buf =
    relativePath buf.config.pathSeperator
        buf.cwd
        buf.path


update : Msg -> Buffer -> ( Buffer, Cmd Msg )
update message buf =
    case message of
        MouseWheel delta ->
            let
                view =
                    buf.view

                scrollTopPx =
                    (view.scrollTopPx + delta)
                        |> max 0
                        |> min ((B.count buf.lines - 2) * buf.view.lineHeight)

                lineHeight =
                    buf.view.lineHeight

                scrollTopDelta =
                    scrollTopPx // lineHeight - buf.view.scrollTop
            in
                { buf
                    | view = { view | scrollTopPx = scrollTopPx }
                }
                    |> applyVimAST False
                        "<mousewheel>"
                        { count = Nothing
                        , edit = Just <| Scroll <| V.ScrollBy scrollTopDelta
                        , register = V.defaultRegister
                        , modeName = getModeName buf.mode
                        , recordMacro = Nothing
                        , recordKeys = ""
                        }

        PressKey key ->
            List.foldl
                (\key ( buf, cmds ) ->
                    let
                        ( buf1, cmd ) =
                            handleKeypress False key buf
                    in
                        ( buf1
                        , if cmd == Cmd.none then
                            cmds
                          else
                            cmd :: cmds
                        )
                )
                ( buf, [] )
                (keymap buf.mode key)
                |> Tuple.mapSecond (List.reverse >> Cmd.batch)

        Resize size ->
            let
                newHeight =
                    getViewHeight size.height
                        buf.view.lineHeight
                        buf.view.statusbarHeight
            in
                buf
                    |> Buf.updateView
                        (\view ->
                            { view
                                | size =
                                    { width =
                                        size.width
                                    , height = newHeight
                                    }
                                , lines =
                                    Buf.getViewLines
                                        view.scrollTop
                                        (view.scrollTop + newHeight + 2)
                                        buf.lines
                                        buf.syntax
                                        |> Buf.fillEmptyViewLines newHeight
                            }
                        )
                    |> cursorScope
                    |> tokenizeBuffer

        ReadClipboard result ->
            case result of
                Ok ( replaying, key, ast, s ) ->
                    buf
                        |> Buf.setRegister "+" (Text s)
                        |> applyVimAST replaying key ast

                Err s ->
                    ( buf, Cmd.none )

        WriteClipboard _ ->
            ( buf, Cmd.none )

        Read result ->
            case result of
                Ok info ->
                    editBuffer info buf

                Err (Http.BadStatus resp) ->
                    case resp.status.code of
                        404 ->
                            jumpTo True
                                { path = resp.body
                                , version = 0
                                , cursor = ( 0, 0 )
                                , content =
                                    Just
                                        ( B.fromString B.lineBreak
                                        , Array.empty
                                        )
                                , syntax = True
                                }
                                buf

                        _ ->
                            ( buf, Cmd.none )

                _ ->
                    ( Buf.errorMessage
                        ("read " ++ shortPath buf ++ " failed")
                        buf
                    , Cmd.none
                    )

        Write result ->
            case result of
                Ok patches ->
                    let
                        -- TODO: add an unified `patch` function for buffer
                        buf1 =
                            buf
                                |> Buf.transaction patches
                                |> Buf.commit
                                |> Buf.updateSavePoint
                                -- keep cursor position
                                |> Buf.setCursor buf.cursor True
                                |> correctCursor
                                |> scrollToCursor
                                |> pairCursor
                                |> Buf.infoMessage
                                    (shortPath buf ++ " Written")

                        syntaxBottom =
                            buf.syntaxDirtyFrom

                        lintCmd buf =
                            if buf1.config.lint then
                                sendLintProject buf1.config.service
                                    buf1.config.pathSeperator
                                    buf1.path
                                    buf1.history.version
                                    buf1.lines
                            else
                                Cmd.none
                    in
                        ( buf1
                        , Cmd.batch
                            [ Doc.setTitle buf1.name
                            , lintCmd buf1
                            , tokenizeBufferCmd buf1
                            ]
                        )

                _ ->
                    ( Buf.errorMessage
                        (shortPath buf ++ " save error")
                        buf
                    , Cmd.none
                    )

        SendLint ->
            if buf.config.lint then
                ( buf
                , sendLintOnTheFly
                    buf.config.service
                    buf.config.pathSeperator
                    buf.path
                    buf.history.version
                    buf.lines
                )
            else
                ( buf, Cmd.none )

        Lint ( path, version ) resp ->
            if
                (path == buf.path)
                    && (version == buf.history.version)
            then
                case resp of
                    Ok items ->
                        let
                            items1 =
                                List.map
                                    (\item ->
                                        { item
                                            | file =
                                                if
                                                    String.isEmpty item.file
                                                        || String.endsWith
                                                            buf.path
                                                            item.file
                                                        || String.endsWith
                                                            "912ec803b2ce49e4a541068d495ab570.txt"
                                                            item.file
                                                then
                                                    buf.path
                                                else
                                                    normalizePath
                                                        buf.config.pathSeperator
                                                        item.file
                                            , region =
                                                let
                                                    ( b, e_ ) =
                                                        item.region

                                                    -- make inclusive
                                                    e =
                                                        if b == e_ then
                                                            e_
                                                        else
                                                            Tuple.mapSecond
                                                                (\x -> (x - 1))
                                                                e_
                                                in
                                                    ( correctPosition
                                                        b
                                                        True
                                                        buf.lines
                                                    , correctPosition
                                                        e
                                                        True
                                                        buf.lines
                                                    )
                                        }
                                    )
                                    items
                        in
                            ( { buf
                                | lint =
                                    { items = items1
                                    , count = List.length items1
                                    }
                                , locationList =
                                    lintErrorToLocationList items1
                              }
                            , Cmd.none
                            )

                    Err _ ->
                        ( buf, Cmd.none )
            else
                ( buf, Cmd.none )

        Tokenized ( path, version ) resp ->
            if
                (path == buf.path)
                    && (version == buf.history.version)
            then
                onTokenized buf resp
            else
                ( buf, Cmd.none )

        SendTokenize ->
            ( buf, tokenizeBufferCmd buf )

        ReadTags result ->
            case result of
                Ok loc ->
                    let
                        last =
                            buf.last

                        saveLastJumpToTag buf =
                            { buf
                                | last =
                                    { last
                                        | jumpToTag =
                                            Just
                                                { path = buf.path
                                                , cursor = buf.cursor
                                                }
                                    }
                            }
                    in
                        buf
                            |> Buf.clearMessage
                            |> saveLastJumpToTag
                            |> jumpToLocation True loc

                _ ->
                    ( Buf.errorMessage "tag not found" buf
                    , Cmd.none
                    )

        SearchResult result ->
            case result of
                Ok s ->
                    jumpTo True
                        { emptyBufferInfo
                            | path = "[Search]"
                            , content = Just ( B.fromString s, Array.empty )
                            , syntax = False
                        }
                        buf

                _ ->
                    ( buf, Cmd.none )

        ListFiles resp ->
            case resp of
                Ok files ->
                    let
                        fileNameWordChars =
                            "/\\-._"
                    in
                        case buf.mode of
                            Ex ({ exbuf } as ex) ->
                                ( setExbuf buf
                                    ex
                                    (case
                                        autoCompleteTarget
                                            fileNameWordChars
                                            exbuf
                                     of
                                        Just ( pos, word ) ->
                                            startAutoComplete
                                                fileNameWordChars
                                                files
                                                pos
                                                word
                                                exbuf

                                        _ ->
                                            startAutoComplete
                                                fileNameWordChars
                                                files
                                                exbuf.cursor
                                                ""
                                                exbuf
                                    )
                                , Cmd.none
                                )

                            _ ->
                                ( buf, Cmd.none )

                Err _ ->
                    ( buf, Cmd.none )

        SetCwd (Ok cwd) ->
            ( Buf.infoMessage cwd { buf | cwd = cwd }, Cmd.none )

        _ ->
            ( buf, Cmd.none )


editBuffer : BufferInfo -> Buffer -> ( Buffer, Cmd Msg )
editBuffer info buf =
    if info.path /= "" && info.content == Nothing then
        ( buf
        , sendReadBuffer buf.config.service
            (Tuple.first info.cursor + buf.view.size.height * 2)
            buf.config.tabSize
            info
        )
    else
        let
            newbuf =
                newBuffer
                    info
                    { buf
                        | jumps = buf.jumps
                        , buffers =
                            (buf.buffers
                                |> Dict.remove info.path
                                |> Dict.insert buf.path
                                    { path = buf.path
                                    , version = buf.history.version
                                    , content =
                                        Just
                                            ( buf.lines, buf.syntax )
                                    , cursor = buf.cursor
                                    , syntax = buf.config.syntax
                                    }
                            )
                        , registers =
                            buf.registers
                                |> Dict.insert "%" (Text info.path)
                                |> (\regs ->
                                        if buf.path == info.path then
                                            regs
                                        else
                                            Dict.insert
                                                "#"
                                                (Text buf.path)
                                                regs
                                   )
                    }
        in
            ( newbuf
            , Cmd.batch
                [ if newbuf.config.lint then
                    sendLintProject newbuf.config.service
                        newbuf.config.pathSeperator
                        newbuf.path
                        newbuf.history.version
                        newbuf.lines
                  else
                    Cmd.none
                , Doc.setTitle newbuf.name
                , tokenizeBufferCmd newbuf
                ]
            )


initCommand : Flags -> Cmd Msg
initCommand =
    sendBoot


getViewHeight : Int -> Int -> Int -> Int
getViewHeight heightPx lineHeightPx statusBarHeight =
    heightPx // lineHeightPx - statusBarHeight


init : Flags -> ( Buffer, Cmd Msg )
init flags =
    let
        { cwd, lineHeight, service, buffers } =
            flags

        { activeBuffer, registers, height, pathSeperator } =
            flags

        viewHeight =
            getViewHeight height lineHeight emptyBuffer.view.statusbarHeight

        cwd1 =
            if String.isEmpty cwd then
                "."
            else
                cwd

        view =
            { emptyView
                | lineHeight = lineHeight
                , size =
                    { width = 1
                    , height = viewHeight
                    }
            }

        activeBuf =
            activeBuffer
                |> Decode.decodeValue bufferInfoDecoder
                |> Result.withDefault emptyBufferInfo

        jumps =
            emptyBuffer.jumps

        ( buf, cmd ) =
            editBuffer
                activeBuf
                { emptyBuffer
                    | view = view
                    , cwd = cwd1
                    , path = activeBuf.path
                    , jumps = jumps
                    , config =
                        { defaultBufferConfig
                            | service = service
                            , pathSeperator = pathSeperator
                        }
                    , registers =
                        Decode.decodeValue registersDecoder registers
                            |> Result.withDefault Dict.empty
                }
    in
        ( { buf
            | buffers =
                buffers
                    |> Decode.decodeValue (Decode.list bufferInfoDecoder)
                    |> Result.withDefault []
                    |> fromListBy .path
          }
        , Cmd.batch <|
            [ Task.perform Resize Win.size
            , cmd
            ]
        )
