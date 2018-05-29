module Update exposing (update, init, Flags)

import Brackets exposing (pairBracket)
import Char
import Task
import Keymap exposing (keymap)
import Window as Win exposing (Size)
import Json.Encode as Encode
import Json.Decode as Decode
import Model exposing (..)
import Message exposing (..)
import Vim.Helper exposing (parseKeys, escapeKey)
import Vim.AST exposing (AST)
import Helper exposing (fromListBy, filename, safeRegex, isSpace, notSpace)
import Vim.Parser exposing (parse)
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Buffer as Buf
import Dict exposing (Dict)
import Parser as P exposing ((|.), (|=), Parser)
import Motion exposing (..)
import Delete exposing (..)
import Insert exposing (..)
import Position exposing (Position)
import PositionClass exposing (findLineFirst)
import Regex as Re
import TextObject exposing (expandTextObject)
import Result
import List
import Tuple
import String
import Service exposing (..)
import Yank exposing (yank)
import Debounce exposing (debounceLint, debounceTokenize)
import Service exposing (sendTokenize)
import Elm.Array as Array
import Document as Doc
import Fuzzy exposing (..)
import Jumps
    exposing
        ( saveJump
        , jumpForward
        , jumpBackward
        , Jumps
        , Location
        , currentLocation
        )
import Range exposing (operatorRanges)


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
            Normal

        V.ModeNameTempNormal ->
            TempNormal

        V.ModeNameInsert ->
            Insert

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
                , autoComplete = Nothing
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
        Normal ->
            V.ModeNameNormal

        Insert ->
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

        newMode =
            if oldModeName == modeName then
                buf.mode
            else
                initMode buf modeName
    in
        { buf | mode = newMode }


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
        Normal ->
            let
                ( y, x ) =
                    buf.cursor

                lastIndent =
                    if oldModeName == V.ModeNameInsert then
                        case buf.last.indent of
                            x ->
                                if B.getLineMaxColumn y buf.lines == x then
                                    x
                                else
                                    0
                    else
                        0

                cursor =
                    if oldModeName == V.ModeNameInsert then
                        if lastIndent > 0 then
                            ( y, 0 )
                        else
                            ( y, max (x - 1) 0 )
                    else
                        ( y
                        , if B.getLineMaxColumn y buf.lines > x then
                            x
                          else
                            max (x - 1) 0
                        )

                buf1 =
                    if lastIndent > 0 then
                        Buf.transaction
                            [ Deletion ( y, 0 ) ( y, lastIndent ) ]
                            buf
                    else
                        buf

                changeColumn =
                    if lineDeltaMotion then
                        False
                    else
                        cursor /= buf.cursor
            in
                buf1
                    |> Buf.setCursor
                        cursor
                        changeColumn
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
                in
                    Buf.setMode
                        (Ex { ex | prefix = prefix1 })
                        buf1

        Insert ->
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


setScrollTop : Int -> Buffer -> Buffer
setScrollTop n buf =
    updateView (\v -> { v | scrollTop = n }) buf


ceilingFromZero : Float -> Int
ceilingFromZero n =
    if n < 0 then
        floor n
    else
        ceiling n


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
                            (expandTextObject buf.config.wordChars
                                textobj
                                around
                                end
                                buf.lines
                            )
                                |> Maybe.map
                                    (\rg ->
                                        let
                                            ( a, b ) =
                                                rg

                                            ( begin1, end1 ) =
                                                expandVisual begin end a b
                                        in
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
                                    y - buf.view.size.height + 1

                                V.ScrollToMiddle ->
                                    y - buf.view.size.height // 2
                    in
                        setScrollTop (scope y1) buf
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
                        |> cmdNone

        Delete rg ->
            buf
                |> delete count register rg
                |> scrollToCursor
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
                        if newn < 0 then
                            scrollTop
                        else if (newn + height) > maxy then
                            max (maxy - height + 1) 0
                        else
                            newn

                n =
                    ceilingFromZero (toFloat height * factor)

                y =
                    lineScope (Tuple.first buf.cursor + n)

                scrollTop =
                    scrollScope view.scrollTop n
            in
                case gotoLine y buf.lines of
                    Just cursor ->
                        buf
                            |> Buf.setCursor cursor True
                            |> setVisualEnd cursor
                            |> setScrollTop scrollTop
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
                    ex.exbuf.lines
                        |> B.toString
                        |> String.dropLeft 1
                        |> flip execute
                            { buf | mode = Ex { ex | autoComplete = Nothing } }

                _ ->
                    ( buf, Cmd.none )

        Join mergeSpaces ->
            buf
                |> join mergeSpaces
                |> cmdNone

        Replace ch ->
            case buf.mode of
                Normal ->
                    let
                        ( y, x ) =
                            buf.cursor
                    in
                        buf
                            |> Buf.transaction
                                [ Deletion buf.cursor ( y, x + 1 ) ]
                            |> insert (V.TextLiteral ch)
                            |> cmdNone

                --|> Buf.setCursor buf.cursor True
                TempNormal ->
                    ( buf, Cmd.none )

                Visual visual ->
                    ( buf, Cmd.none )

                _ ->
                    ( buf, Cmd.none )

        ToggleTip ->
            buf
                |> Buf.setShowTip (not buf.view.showTip)
                |> cmdNone

        SelectAutoComplete dir ->
            case buf.mode of
                Ex ex ->
                    case ex.autoComplete of
                        Just auto ->
                            let
                                { matches, select } =
                                    auto

                                s =
                                    B.toString ex.exbuf.lines

                                newSelect =
                                    (select
                                        + if dir == V.Forward then
                                            1
                                          else
                                            -1
                                    )
                                        % Array.length matches

                                targetSelected =
                                    newSelect == Array.length matches - 1

                                scrollTop =
                                    if targetSelected then
                                        auto.scrollTop
                                    else if newSelect < auto.scrollTop then
                                        newSelect
                                    else if newSelect >= auto.scrollTop + 15 then
                                        newSelect - 15 + 1
                                    else
                                        auto.scrollTop

                                txt =
                                    case Array.get newSelect matches of
                                        Just m ->
                                            m.text

                                        _ ->
                                            ""

                                exbuf =
                                    if String.startsWith ":e " s then
                                        Buf.transaction
                                            [ Deletion ( 0, 3 ) ( 1, 0 )
                                            , Insertion ( 0, 3 ) <|
                                                B.fromString txt
                                            ]
                                            ex.exbuf
                                    else
                                        ex.exbuf
                            in
                                ( { buf
                                    | mode =
                                        Ex
                                            { ex
                                                | exbuf = exbuf
                                                , autoComplete =
                                                    Just
                                                        { auto
                                                            | select = newSelect
                                                            , scrollTop =
                                                                scrollTop
                                                        }
                                            }
                                  }
                                , Cmd.none
                                )

                        _ ->
                            ( buf, Cmd.none )

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
                    Just { path, cursor } ->
                        jumpTo False
                            (buf.buffers
                                |> Dict.get path
                                |> Maybe.withDefault
                                    { path = path
                                    , cursor = cursor
                                    , scrollTop = 0
                                    , content = Nothing
                                    }
                            )
                            { buf | jumps = jumps }

                    _ ->
                        ( buf, Cmd.none )

        JumpLastBuffer ->
            case Dict.get "#" buf.registers of
                Just reg ->
                    let
                        path =
                            registerString reg

                        --|> Debug.log "JumpLastBuffer"
                        info =
                            buf.buffers
                                |> Dict.get path
                                |> Maybe.withDefault
                                    { path = path
                                    , cursor = ( 0, 0 )
                                    , scrollTop = 0
                                    , content = Nothing
                                    }
                    in
                        jumpTo True info buf

                _ ->
                    ( buf, Cmd.none )

        JumpToTag ->
            case wordStringUnderCursor buf of
                Just ( _, s ) ->
                    ( buf
                    , sendReadTags buf.config.service
                        buf.path
                        (Maybe.withDefault 1 count - 1)
                        s
                    )

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
                                    , case (Debug.log "ints" ints) of
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
                                let
                                    info =
                                        buf.buffers
                                            |> Dict.get path
                                            |> Maybe.withDefault
                                                { path = path
                                                , cursor = cursor
                                                , scrollTop = 0
                                                , content = Nothing
                                                }
                                in
                                    jumpTo True { info | cursor = cursor } buf

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
                            ( gotoLine y buf1.lines
                                |> Maybe.map
                                    (\pos ->
                                        Buf.setCursor pos True buf1
                                    )
                                |> Maybe.withDefault buf
                            , Cmd.none
                            )

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

        _ ->
            ( buf, Cmd.none )


{-| scroll to ensure pos it is insdie viewport
-}
scrollTo : Position -> Buffer -> Buffer
scrollTo pos ({ view, lines } as buf) =
    let
        ( y, _ ) =
            pos

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
        setScrollTop scrollTop buf


scrollToCursor : Buffer -> Buffer
scrollToCursor buf =
    scrollTo buf.cursor buf


{-| move cursor ensure cursor is insdie viewport
-}
cursorScope : Buffer -> Buffer
cursorScope ({ view, cursor, lines } as buf) =
    let
        ( y, _ ) =
            cursor

        maxy =
            min
                (view.scrollTop + view.size.height - 1)
                (B.count lines - 1)

        miny =
            min view.scrollTop maxy

        y1 =
            y |> min maxy |> max miny
    in
        if y == y1 then
            buf
        else
            case gotoLine y1 lines of
                Just cursor ->
                    buf
                        |> Buf.setCursor cursor True
                        |> setVisualEnd cursor

                _ ->
                    buf


newBuffer : BufferInfo -> Buffer -> Buffer
newBuffer info buf =
    let
        { cursor, scrollTop, path, content } =
            info

        ( name, ext ) =
            filename path

        --|> Debug.log "path"
        config =
            Buf.configs
                |> Dict.get ext
                |> Maybe.withDefault defaultBufferConfig

        lines =
            content
                |> Maybe.withDefault (B.toString emptyBuffer.lines)
                |> B.fromStringExpandTabs buf.config.tabSize 0
    in
        { buf
            | lines = lines
            , config =
                { config
                    | service = buf.config.service
                    , syntaxService = buf.config.syntaxService
                }
            , view =
                { emptyView
                    | size = buf.view.size
                    , lineHeight = buf.view.lineHeight
                    , scrollTop = min (B.count lines - 1) scrollTop
                }
            , cursor = cursor
            , lint = { items = [], count = buf.lint.count }
            , cursorColumn = Tuple.second cursor
            , path = path
            , name = name ++ ext
            , history = emptyBufferHistory
            , syntax = Array.empty
        }
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


applyEdit : Maybe Int -> Maybe Operator -> String -> Buffer -> ( Buffer, Cmd Msg )
applyEdit count edit register buf =
    case edit of
        Just operator ->
            case buf.mode of
                Ex ex ->
                    let
                        ( buf1, cmd ) =
                            runOperator count register operator buf
                    in
                        case buf1.mode of
                            Ex newex ->
                                if isExEditing operator then
                                    let
                                        ( newex1, cmd1 ) =
                                            exAutoComplete
                                                buf1.config.service
                                                newex
                                    in
                                        ( { buf1 | mode = Ex newex1 }
                                        , Cmd.batch [ cmd, cmd1 ]
                                        )
                                else
                                    ( buf1, cmd )

                            _ ->
                                ( buf1, cmd )

                _ ->
                    runOperator count register operator buf

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

        scrollTop =
            let
                n =
                    Tuple.first cursor
                        - (Tuple.first buf.cursor - buf.view.scrollTop)
                        |> max 0
            in
                if n < buf.view.size.height then
                    0
                else
                    n
    in
        if path == buf.path then
            let
                buf1 =
                    { buf | jumps = jumps }
                        |> Buf.setCursor cursor True
                        |> setScrollTop scrollTop

                bottom =
                    buf1.view.scrollTop
                        + buf1.view.size.height
                        + buf1.config.tokenizeLinesAhead

                syntaxBottom =
                    Array.length buf.syntax
            in
                ( buf1
                , if bottom <= syntaxBottom then
                    Cmd.none
                  else
                    debounceTokenize
                        100
                        buf1.path
                        buf1.history.version
                        syntaxBottom
                        (buf1.lines
                            |> B.sliceLines
                                syntaxBottom
                                bottom
                            |> B.toString
                        )
                )
        else
            editBuffer
                { info | scrollTop = scrollTop }
                { buf | jumps = jumps }


execute : String -> Buffer -> ( Buffer, Cmd Msg )
execute s buf =
    case String.split " " s of
        [ "e", path ] ->
            let
                info =
                    buf.buffers
                        |> Dict.get path
                        |> Maybe.withDefault
                            { path = path
                            , cursor = ( 0, 0 )
                            , scrollTop = 0
                            , content = Nothing
                            }
            in
                jumpTo True info buf

        [ "w" ] ->
            ( buf
            , sendSaveBuffer buf.config.service buf.path <|
                B.toString buf.lines
            )

        [ "w", path ] ->
            ( buf
            , sendSaveBuffer buf.config.service path <|
                B.toString buf.lines
            )

        [ "ll" ] ->
            case List.head buf.locationList of
                Just loc ->
                    let
                        { path, cursor } =
                            loc

                        info =
                            buf.buffers
                                |> Dict.get path
                                |> Maybe.map
                                    (\info1 -> { info1 | cursor = cursor })
                                |> Maybe.withDefault
                                    { path = path
                                    , cursor = cursor
                                    , scrollTop = 0
                                    , content = Nothing
                                    }
                    in
                        jumpTo True info buf

                _ ->
                    ( buf, Cmd.none )

        [ "f", s ] ->
            ( buf, sendFind buf.config.service s )

        _ ->
            ( buf, Cmd.none )


exAutoComplete : String -> ExMode -> ( ExMode, Cmd Msg )
exAutoComplete service ex =
    let
        { exbuf, autoComplete } =
            ex

        s =
            B.toString exbuf.lines
    in
        if String.startsWith ":e " s then
            case autoComplete of
                Just auto ->
                    let
                        { source } =
                            auto

                        target =
                            exbuf
                                |> autoCompleteTarget

                        matches =
                            target
                                |> fuzzyMatch source
                                |> Array.fromList
                    in
                        ( { ex
                            | autoComplete =
                                Just
                                    { auto
                                        | matches =
                                            Array.push
                                                { text = target, matches = [] }
                                                matches
                                    }
                          }
                        , Cmd.none
                        )

                _ ->
                    let
                        newex =
                            Ex
                                { ex
                                    | autoComplete =
                                        Just
                                            { source = []
                                            , matches = Array.empty
                                            , select = -1
                                            , scrollTop = 0
                                            }
                                }
                    in
                        ( ex, sendListFiles service )
        else
            ( ex, Cmd.none )


autoCompleteTarget : Buffer -> String
autoCompleteTarget exbuf =
    exbuf.lines
        |> B.getLine 0
        |> Maybe.andThen
            (String.split " "
                >> List.tail
            )
        |> Maybe.andThen
            (List.filter (String.isEmpty >> not)
                >> List.head
            )
        |> Maybe.withDefault ""


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
    in
        case serviceBeforeApplyVimAST replaying key ast buf of
            Just cmd ->
                ( buf, cmd )

            _ ->
                buf
                    |> cacheVimAST cacheKey cacheVal
                    |> setContinuation continuation
                    |> applyVimAST replaying key ast


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
                (oldBuf.cursor /= buf.cursor)
                    || (oldBuf.view.scrollTop /= buf.view.scrollTop)
                    || (oldBuf.view.size /= buf.view.size)
                    || (oldBuf.lines /= buf.lines)
                    || (oldBuf.syntax /= buf.syntax)
            then
                let
                    view =
                        buf.view

                    matchedCursor =
                        pairBracket
                            buf.view.scrollTop
                            (buf.view.scrollTop + buf.view.size.height)
                            buf.lines
                            buf.syntax
                            buf.cursor
                in
                    { buf | view = { view | matchedCursor = matchedCursor } }
            else
                buf

        doTokenize oldBuf ( buf, cmds ) =
            let
                oldBottom =
                    oldBuf.view.scrollTop + oldBuf.view.size.height

                newBottom =
                    let
                        n =
                            buf.view.scrollTop + buf.view.size.height
                    in
                        case buf.mode of
                            Ex { prefix, visual } ->
                                case prefix of
                                    ExSearch { match } ->
                                        case match of
                                            Just ( begin, _ ) ->
                                                Tuple.first begin
                                                    + buf.view.size.height

                                            _ ->
                                                n

                                    _ ->
                                        n

                            _ ->
                                n
            in
                case buf.syntaxDirtyFrom of
                    Just y ->
                        --let
                        --    _ =
                        --        Debug.log "y" y
                        --in
                        ( buf
                        , sendTokenizeLine
                            buf.config.syntaxService
                            { version = buf.history.version
                            , line = y
                            , lines =
                                (buf.lines
                                    |> B.sliceLines y (y + 1)
                                    |> B.toString
                                )
                            , path = buf.path
                            }
                            :: cmds
                        )

                    --debounceTokenize 100
                    --    y
                    --    (buf.lines
                    --        |> B.sliceLines y (newBottom + 1)
                    --        |> B.toString
                    --    )
                    _ ->
                        if oldBottom == newBottom then
                            ( buf, cmds )
                        else if newBottom >= Array.length buf.syntax then
                            ( buf
                            , debounceTokenize
                                100
                                buf.path
                                buf.history.version
                                (Array.length buf.syntax)
                                (buf.lines
                                    |> B.sliceLines
                                        (Array.length buf.syntax)
                                        (newBottom
                                            + buf.config.tokenizeLinesAhead
                                        )
                                    |> B.toString
                                )
                                :: cmds
                            )
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
                ( buf, debounceLint 500 :: cmds )
            else
                ( buf, cmds )

        clearSyntaxDirtyFrom buf =
            { buf | syntaxDirtyFrom = Nothing }
    in
        buf
            |> applyEdit count edit register
            |> Tuple.mapFirst
                (updateMode modeName
                    >> modeChanged replaying key oldModeName lineDeltaMotion
                    >> scrollToCursor
                    >> saveDotRegister replaying
                    >> setMatchedCursor buf
                )
            |> Tuple.mapSecond List.singleton
            |> doLint buf
            |> doSetTitle buf
            |> doTokenize buf
            |> Tuple.mapFirst clearSyntaxDirtyFrom
            |> Tuple.mapSecond Cmd.batch


onTokenized : Buffer -> Result error TokenizeResponse -> ( Buffer, Cmd Msg )
onTokenized buf resp =
    case resp of
        Ok payload ->
            case payload of
                TokenizeSuccess path version begin syntax ->
                    ( if
                        (path == buf.path)
                            && (version == buf.history.version)
                      then
                        let
                            syntax1 =
                                buf.syntax
                                    |> Array.slice 0 begin
                                    |> flip Array.append syntax

                            view =
                                buf.view
                        in
                            { buf
                                | syntax = syntax1
                                , view =
                                    { view
                                        | matchedCursor =
                                            pairBracket
                                                buf.view.scrollTop
                                                (buf.view.scrollTop + buf.view.size.height)
                                                buf.lines
                                                syntax1
                                                buf.cursor
                                    }
                            }
                      else
                        buf
                    , Cmd.none
                    )

                LineTokenizeSuccess path version begin tokens ->
                    ( if
                        (path == buf.path)
                            && (version == buf.history.version)
                      then
                        { buf | syntax = Array.set begin tokens buf.syntax }
                      else
                        buf
                    , debounceTokenize
                        200
                        buf.path
                        buf.history.version
                        (begin + 1)
                        (buf.lines
                            |> B.sliceLines
                                (begin + 1)
                                (buf.view.scrollTop
                                    + buf.view.size.height
                                    + 1
                                )
                            |> B.toString
                        )
                    )

                TokenizeCacheMiss ->
                    ( buf
                    , sendTokenize
                        buf.config.syntaxService
                        { path = buf.path
                        , version = buf.history.version
                        , line = 0
                        , lines =
                            buf.lines
                                |> B.sliceLines
                                    0
                                    (buf.view.scrollTop
                                        + buf.view.size.height
                                        + buf.config.tokenizeLinesAhead
                                    )
                                |> B.toString
                        }
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


update : Msg -> Model -> ( Model, Cmd Msg )
update message buf =
    case message of
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
                h =
                    (size.height // buf.view.lineHeight)
                        - buf.view.statusbarHeight

                w =
                    size.width

                syntaxLines =
                    Array.length buf.syntax
            in
                ( buf
                    |> updateView
                        (\view ->
                            { view | size = { width = w, height = h } }
                        )
                    |> cursorScope
                , if
                    (syntaxLines > 0)
                        && (buf.view.scrollTop + h > syntaxLines)
                  then
                    debounceTokenize
                        200
                        buf.path
                        buf.history.version
                        (Array.length buf.syntax)
                        (buf.lines
                            |> B.sliceLines
                                (Array.length buf.syntax)
                                (buf.view.scrollTop
                                    + h
                                    + buf.config.tokenizeLinesAhead
                                )
                            |> B.toString
                        )
                  else
                    Cmd.none
                )

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

                Err s ->
                    ( buf, Cmd.none )

        Write result ->
            case result of
                Ok s ->
                    let
                        buf1 =
                            if s == "" then
                                Buf.updateSavePoint buf
                            else
                                let
                                    n =
                                        B.count buf.lines

                                    patches =
                                        [ Deletion ( 0, 0 ) ( n, 0 )
                                        , Insertion ( 0, 0 ) (B.fromString s)
                                        ]
                                in
                                    buf
                                        |> Buf.transaction patches
                                        |> Buf.commit
                                        |> Buf.updateSavePoint
                                        |> Buf.setCursor buf.cursor True
                                        |> cursorScope
                    in
                        ( buf1
                        , Cmd.batch
                            [ sendTokenize
                                buf1.config.syntaxService
                                { path = buf1.path
                                , version = buf1.history.version
                                , line = 0
                                , lines =
                                    buf1.lines
                                        |> B.sliceLines 0
                                            (buf1.view.scrollTop
                                                + buf1.view.size.height
                                                + 1
                                            )
                                        |> B.toString
                                }
                            , Doc.setTitle buf1.name
                            , if buf1.config.lint then
                                sendLintProject buf1.config.service buf1.path
                              else
                                Cmd.none
                            ]
                        )

                Err err ->
                    ( buf, Cmd.none )

        Edit info ->
            ( buf, sendEditBuffer buf.config.service info )

        SendLint ->
            if buf.config.lint then
                ( buf
                , sendLintOnTheFly
                    buf.config.service
                    buf.path
                    (B.toString buf.lines)
                )
            else
                ( buf, Cmd.none )

        LintOnTheFly resp ->
            case resp of
                Ok errors ->
                    let
                        items =
                            List.map (\item -> { item | file = buf.path })
                                errors
                    in
                        ( { buf
                            | lint =
                                { items = items
                                , count = List.length items
                                }
                            , locationList =
                                lintErrorToLocationList items
                          }
                        , Cmd.none
                        )

                Err _ ->
                    ( buf, Cmd.none )

        Lint resp ->
            case resp of
                Ok items ->
                    let
                        items1 =
                            List.filter
                                (\item ->
                                    String.endsWith
                                        (String.toLower buf.path)
                                        (String.toLower item.file)
                                )
                                items

                        showTip =
                            not (List.isEmpty items1)
                    in
                        ( { buf
                            | lint =
                                { items = items1
                                , count = List.length items
                                }
                            , locationList =
                                lintErrorToLocationList items
                          }
                        , Cmd.none
                        )

                Err _ ->
                    ( buf, Cmd.none )

        Tokenized resp ->
            onTokenized buf resp

        SendTokenize req ->
            ( buf
            , sendTokenize
                buf.config.syntaxService
                req
            )

        ReadTags result ->
            case result of
                Ok loc ->
                    jumpTo True
                        { path = loc.path
                        , cursor = loc.cursor
                        , scrollTop = 0
                        , content = Nothing
                        }
                        buf

                _ ->
                    ( buf, Cmd.none )

        FindResult result ->
            case result of
                Ok s ->
                    jumpTo True
                        { path = "[Find]"
                        , cursor = ( 0, 0 )
                        , scrollTop = 0
                        , content = Just s
                        }
                        buf

                _ ->
                    ( buf, Cmd.none )

        ListFiles resp ->
            case resp of
                Ok files ->
                    case buf.mode of
                        Ex ex ->
                            let
                                autoComplete =
                                    Just
                                        { source = files
                                        , matches =
                                            ex.exbuf
                                                |> autoCompleteTarget
                                                |> fuzzyMatch files
                                                |> Array.fromList
                                                |> Array.push
                                                    { text =
                                                        autoCompleteTarget
                                                            ex.exbuf
                                                    , matches = []
                                                    }
                                        , select = -1
                                        , scrollTop = 0
                                        }
                            in
                                ( { buf
                                    | mode =
                                        Ex
                                            { ex
                                                | autoComplete =
                                                    autoComplete
                                            }
                                  }
                                , Cmd.none
                                )

                        _ ->
                            ( buf, Cmd.none )

                Err _ ->
                    ( buf, Cmd.none )

        _ ->
            ( buf, Cmd.none )


editBuffer : BufferInfo -> Buffer -> ( Buffer, Cmd Msg )
editBuffer info buf =
    if info.path /= "" && info.content == Nothing then
        ( buf, sendEditBuffer buf.config.service info )
    else
        let
            view =
                buf.view

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
                                    , content = buf.lines |> B.toString |> Just
                                    , scrollTop = buf.view.scrollTop
                                    , cursor = buf.cursor
                                    }
                            )
                        , registers =
                            buf.registers
                                |> Dict.insert "%" (Text info.path)
                                |> (\regs ->
                                        if buf.path == info.path then
                                            regs
                                        else
                                            Dict.insert "#" (Text buf.path) regs
                                   )
                    }
        in
            ( newbuf
            , Cmd.batch
                ((if newbuf.config.lint then
                    sendLintProject newbuf.config.service newbuf.path
                  else
                    Cmd.none
                 )
                    :: Doc.setTitle newbuf.name
                    :: [ sendTokenize
                            buf.config.syntaxService
                            { path = newbuf.path
                            , version = newbuf.history.version
                            , line = 0
                            , lines =
                                newbuf.lines
                                    |> B.sliceLines 0
                                        (newbuf.view.scrollTop
                                            + newbuf.view.size.height
                                            + newbuf.config.tokenizeLinesAhead
                                        )
                                    |> B.toString
                            }
                       ]
                )
            )


type alias Flags =
    { lineHeight : Int
    , service : String
    , syntaxService : String
    , buffers : Encode.Value
    , activeBuffer : Encode.Value
    , registers : Encode.Value
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        { lineHeight, service, syntaxService, buffers, activeBuffer, registers } =
            flags

        _ =
            Debug.log "flags" flags

        view =
            emptyBuffer.view

        activeBuf =
            activeBuffer
                |> Decode.decodeValue bufferInfoDecoder
                |> Result.withDefault
                    { path = ""
                    , content = Nothing
                    , scrollTop = 0
                    , cursor = ( 0, 0 )
                    }

        jumps =
            emptyBuffer.jumps

        ( buf, cmd ) =
            editBuffer
                activeBuf
                { emptyBuffer
                    | view = { view | lineHeight = lineHeight }
                    , jumps = jumps |> Debug.log "jumps"
                    , config =
                        { defaultBufferConfig
                            | service = service
                            , syntaxService = syntaxService
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
