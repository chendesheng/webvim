module Update exposing (update)

import Model exposing (..)
import Message exposing (..)
import Vim.Helper exposing (keyParser, escapeKey)
import Vim.Parser exposing (parse)
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Buffer as Buf
import Dict
import Parser as P exposing ((|.), (|=), Parser)
import Motion exposing (..)
import Delete exposing (..)
import Insert exposing (..)
import Position exposing (Position)
import Regex as Re
import TextObject exposing (expandTextObject)
import Result
import List
import Tuple
import String
import Service exposing (..)
import Persistent exposing (..)
import Yank exposing (yank)
import Debounce exposing (debounceLint, debounceTokenize)
import Service exposing (sendTokenize)
import Elm.Array as Array
import Document as Doc
import Fuzzy exposing (..)
import Jumps exposing (saveCursorPosition, jump)


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
                V.LineDelta _ ->
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
                                let
                                    s =
                                        exbuf.lines
                                            |> B.toString
                                            |> String.dropLeft 1

                                    re =
                                        Re.regex s
                                            |> Re.caseInsensitive
                                in
                                    if String.isEmpty s then
                                        ExSearch
                                            { search | match = Nothing }
                                    else
                                        ExSearch
                                            { search
                                                | match =
                                                    matchString forward
                                                        re
                                                        buf.cursor
                                                        buf.lines
                                            }

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


scrollToLine : Int -> Buffer -> Buffer
scrollToLine n buf =
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


runOperator : String -> Operator -> Buffer -> ( Buffer, Cmd Msg )
runOperator register operator buf =
    case operator of
        Move md mo ->
            motion md mo buf

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
            let
                scrollTop =
                    let
                        y =
                            Tuple.first buf.cursor
                    in
                        case value of
                            V.ScrollBy n ->
                                buf.view.scrollTop + n

                            V.ScrollToTop ->
                                y

                            V.ScrollToBottom ->
                                y - buf.view.size.height + 1

                            V.ScrollToMiddle ->
                                y - buf.view.size.height // 2

                scope n =
                    n
                        |> max 0
                        |> min (B.countLines buf.lines - 1)
            in
                buf
                    |> scrollToLine (scope scrollTop)
                    |> cursorScope
                    |> cmdNone

        InsertString s ->
            buf
                |> insert s
                |> cmdNone

        Delete rg ->
            buf
                |> delete register rg
                |> cursorScope
                |> cmdNone

        Yank rg ->
            buf
                |> yank register rg
                |> cmdNone

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
                        |> min (B.countLines buf.lines - 1)

                scrollScope scrollTop n =
                    let
                        newn =
                            scrollTop + n

                        maxy =
                            B.countLines buf.lines - 1
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
                            |> scrollToLine scrollTop
                            |> cmdNone

                    Nothing ->
                        ( buf, Cmd.none )

        Put forward ->
            Dict.get register buf.registers
                |> Maybe.map
                    (\s ->
                        case buf.mode of
                            Ex ({ exbuf } as ex) ->
                                Buf.setMode
                                    (Ex
                                        { ex
                                            | exbuf =
                                                Buf.putString
                                                    forward
                                                    s
                                                    exbuf
                                        }
                                    )
                                    buf

                            _ ->
                                Buf.putString forward s buf
                    )
                |> Maybe.withDefault buf
                |> cmdNone

        RepeatLastOperator ->
            replayKeys buf.dotRegister buf

        RepeatLastInsert ->
            replayKeys buf.last.inserts buf

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
                    ( { buf | mode = Ex { ex | autoComplete = Nothing } }
                    , ex.exbuf.lines
                        |> B.toString
                        |> String.dropLeft 1
                        |> flip execute buf
                    )

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

                        setCursor =
                            if ch == B.lineBreak then
                                identity
                            else
                                Buf.setCursor buf.cursor True
                    in
                        buf
                            |> Buf.transaction
                                [ Deletion buf.cursor ( y, x + 1 ) ]
                            |> insert (V.TextLiteral ch)
                            |> setCursor
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
            ( buf, jump isForward )

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
        scrollToLine scrollTop buf


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
                (B.countLines lines - 1)

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


isExEditing : Operator -> Bool
isExEditing op =
    case op of
        Delete _ ->
            True

        InsertString _ ->
            True

        _ ->
            False


applyEdit : Maybe Operator -> String -> Buffer -> ( Buffer, Cmd Msg )
applyEdit edit register buf =
    case edit of
        Just operator ->
            case buf.mode of
                Ex ex ->
                    let
                        ( buf1, cmd ) =
                            runOperator register operator buf
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
                    runOperator register operator buf

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
                    |> P.run (P.repeat P.zeroOrMore keyParser)
                    |> Result.withDefault []

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


execute : String -> Buffer -> Cmd Msg
execute s buf =
    case String.split " " s of
        [ "e", path ] ->
            Cmd.batch
                [ saveCursorPosition buf.cursor
                , getBuffer path
                ]

        [ "w" ] ->
            sendSaveBuffer buf.config.service buf.path <|
                B.toString buf.lines

        [ "w", path ] ->
            sendSaveBuffer buf.config.service path <|
                B.toString buf.lines

        _ ->
            Cmd.none


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
        |> Maybe.andThen List.head
        |> Maybe.withDefault ""


handleKeypress : Bool -> Key -> Buffer -> ( Buffer, Cmd Msg )
handleKeypress replaying key buf =
    let
        oldBottom =
            buf.view.scrollTop + buf.view.size.height

        cacheKey =
            ( buf.continuation, key )

        (( ast, continuation ) as cacheVal) =
            case Dict.get cacheKey buf.vimASTCache of
                Just resp ->
                    resp

                _ ->
                    parse buf.continuation key

        { edit, modeName, register, recordKeys } =
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

        saveDotRegister buf =
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

        --bind b ( buf, cmd ) =
        --    ( b buf, cmd )
        bind b a =
            (\buf ->
                let
                    ( buf1, cmd1 ) =
                        a buf

                    buf2 =
                        b buf1
                in
                    ( buf2, cmd1 )
            )

        ( buf1, todo ) =
            (cacheVimAST cacheKey cacheVal
                >> setContinuation continuation
                >> applyEdit edit register
                |> bind
                    (updateMode modeName
                        >> modeChanged replaying key oldModeName lineDeltaMotion
                        >> scrollToCursor
                        >> saveDotRegister
                    )
            )
                buf

        newBottom =
            buf1.view.scrollTop + buf1.view.size.height

        getPatchLine =
            Maybe.andThen
                (.patches
                    >> List.head
                    >> Maybe.map
                        (\patch ->
                            Tuple.first
                                (case patch of
                                    Insertion pos _ ->
                                        pos

                                    Deletion pos _ ->
                                        pos
                                )
                        )
                )

        patchesLength undo =
            undo
                |> Maybe.map (.patches >> List.length)
                |> Maybe.withDefault 0

        doTokenize =
            case buf1.syntaxDirtyFrom of
                Just y ->
                    --let
                    --    _ =
                    --        Debug.log "y" y
                    --in
                    sendTokenizeLine
                        buf1.config.syntaxService
                        { version = buf1.history.version
                        , line = y
                        , lines =
                            (buf1.lines
                                |> B.sliceLines y (y + 1)
                                |> B.toString
                            )
                        , path = buf.path
                        }

                --debounceTokenize 100
                --    y
                --    (buf1.lines
                --        |> B.sliceLines y (newBottom + 1)
                --        |> B.toString
                --    )
                _ ->
                    if oldBottom == newBottom then
                        Cmd.none
                    else if newBottom >= Array.length buf1.syntax then
                        debounceTokenize
                            100
                            buf1.path
                            buf1.history.version
                            (Array.length buf1.syntax)
                            (buf1.lines
                                |> B.sliceLines
                                    (Array.length buf1.syntax)
                                    (newBottom
                                        + buf1.config.tokenizeLinesAhead
                                    )
                                |> B.toString
                            )
                    else
                        Cmd.none

        doSetTitle =
            if Buf.isDirty buf /= Buf.isDirty buf1 then
                let
                    prefix =
                        if Buf.isDirty buf1 then
                            "• "
                        else
                            ""
                in
                    Doc.setTitle (prefix ++ buf1.name)
            else
                Cmd.none

        doLint =
            if Buf.isEditing buf buf1 then
                debounceLint 500
            else
                Cmd.none
    in
        ( { buf1 | syntaxDirtyFrom = Nothing }
        , Cmd.batch
            ([ todo
             , encodeBuffer buf1 |> saveBuffer
             , doLint
             , doSetTitle
             , doTokenize
             ]
            )
        )


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
                        { buf
                            | syntax =
                                buf.syntax
                                    |> Array.slice 0 begin
                                    |> flip Array.append syntax
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


update : Msg -> Model -> ( Model, Cmd Msg )
update message buf =
    case message of
        PressKey key ->
            handleKeypress False key buf

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

        Read result ->
            case result of
                Ok info ->
                    let
                        newbuf =
                            Buf.newBuffer
                                info
                                buf.config.service
                                buf.config.syntaxService
                                buf.view.size
                                buf.view.lineHeight
                    in
                        ( newbuf
                        , Cmd.batch
                            ((if newbuf.config.lint then
                                sendLintProject buf.path buf.config.service
                              else
                                Cmd.none
                             )
                                :: Doc.setTitle newbuf.name
                                :: [ sendTokenize
                                        buf.config.syntaxService
                                        { path = Debug.log "path" newbuf.path
                                        , version = buf.history.version
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
                                        B.countLines buf.lines

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
                            , Doc.setTitle buf.name
                            , if buf.config.lint then
                                sendLintProject buf.path buf.config.service
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
                Ok items ->
                    ( { buf
                        | lintItems = items
                        , lintErrorsCount = List.length items
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
                                    item.file
                                        |> String.dropLeft 2
                                        |> String.toLower
                                        |> flip String.endsWith buf.path
                                )
                                items

                        showTip =
                            not (List.isEmpty items1)
                    in
                        ( { buf
                            | lintItems = items1
                            , lintErrorsCount = List.length items
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

        OnJump pos ->
            ( buf
                |> Buf.setCursor pos True
                |> scrollToCursor
            , if
                (buf.view.scrollTop + buf.view.size.height)
                    > Array.length buf.syntax
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
                                + buf.view.size.height
                                + buf.config.tokenizeLinesAhead
                            )
                        |> B.toString
                    )
              else
                Cmd.none
            )

        _ ->
            ( buf, Cmd.none )
