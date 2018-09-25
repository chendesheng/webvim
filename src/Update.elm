module Update exposing (update, init, initCommand, initMode)

import Http
import Task
import Update.Keymap exposing (mapKeys)
import Json.Decode as Decode
import Model exposing (..)
import Update.Message exposing (..)
import Vim.Helper exposing (parseKeys, escapeKey)
import Vim.AST exposing (AST)
import Helper.Helper
    exposing
        ( fromListBy
        , normalizePath
        , nthList
        , regexWith
        , inc
        , replaceHomeDir
        )
import Vim.Parser exposing (parse)
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Update.Buffer as Buf
import Dict exposing (Dict)
import Update.Motion exposing (..)
import Update.Delete exposing (..)
import Update.Insert exposing (..)
import Regex as Re
import Update.Service exposing (..)
import Update.Yank exposing (yank, put)
import Helper.Debounce exposing (debounceLint, debounceTokenize)
import Array as Array exposing (Array)
import Internal.Jumps exposing (Location)
import Update.AutoComplete exposing (..)
import Update.CaseOperator exposing (applyCaseOperator)
import Update.Replace exposing (applyReplace)
import Update.Indent exposing (applyIndent)
import Update.Increase exposing (increaseNumber)
import Update.Select exposing (select)
import Update.Cursor exposing (..)
import Update.Jump exposing (..)
import Update.Range exposing (visualRegions)
import Browser.Dom as Dom
import Process


stringToPrefix : String -> ExPrefix
stringToPrefix prefix =
    case prefix of
        "/" ->
            ExSearch { forward = True, match = Nothing, highlights = [] }

        "?" ->
            ExSearch { forward = False, match = Nothing, highlights = [] }

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
                , visual =
                    case mode of
                        Visual visual ->
                            Just visual

                        _ ->
                            Nothing
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

        setCursor mode buf_ =
            if oldModeName == V.ModeNameVisual V.VisualBlock then
                case mode of
                    Insert { startCursor } ->
                        if startCursor /= buf_.cursor then
                            Buf.setCursor startCursor True buf_
                        else
                            buf_

                    _ ->
                        buf_
            else
                buf_

        buf1 =
            { buf | mode = newMode }
                |> setCursor newMode

        view =
            buf1.view

        scrollFrom =
            Buf.finalScrollTop buf

        scrollTo =
            Buf.finalScrollTop buf1

        ime =
            if oldModeName /= modeName then
                let
                    oldIme =
                        buf.ime
                in
                    case modeName of
                        V.ModeNameInsert ->
                            { oldIme | isActive = True }

                        _ ->
                            { oldIme | isActive = False }
            else
                buf.ime
    in
        { buf1
            | last = { last | motionFailed = False }
            , ime = ime
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


repeatInserts : Mode -> Buffer -> List Patch
repeatInserts oldMode buf =
    case oldMode of
        Insert { startCursor, visual } ->
            case visual of
                Just { tipe, begin, end } ->
                    if
                        (startCursor <= buf.cursor)
                            && (Tuple.first startCursor == Tuple.first buf.cursor)
                    then
                        case tipe of
                            V.VisualBlock ->
                                let
                                    ( by, bx ) =
                                        begin

                                    ( ey, ex ) =
                                        end

                                    minY =
                                        min by ey

                                    maxY =
                                        max by ey

                                    minX =
                                        min bx ex

                                    inserts =
                                        (B.sliceRegion startCursor
                                            ( Tuple.first buf.cursor
                                            , Tuple.second buf.cursor + 1
                                            )
                                            buf.lines
                                        )

                                    ( _, col ) =
                                        startCursor
                                in
                                    if col <= minX then
                                        buf.lines
                                            |> visualRegions False
                                                tipe
                                                begin
                                                end
                                            |> List.tail
                                            |> Maybe.withDefault []
                                            |> List.map
                                                (\( ( y, _ ), _ ) -> y)
                                            |> List.reverse
                                            |> List.map
                                                (\y ->
                                                    Insertion ( y, col ) inserts
                                                )
                                    else
                                        List.range (minY + 1) maxY
                                            |> List.reverse
                                            |> List.map
                                                (\y ->
                                                    let
                                                        maxcol =
                                                            B.getLineMaxColumn y buf.lines
                                                    in
                                                        if col > maxcol then
                                                            Insertion
                                                                ( y, maxcol )
                                                                (inserts
                                                                    |> B.toString
                                                                    |> ((++) <| String.repeat (col - maxcol) " ")
                                                                    |> B.fromString
                                                                )
                                                        else
                                                            Insertion
                                                                ( y, col )
                                                                inserts
                                                )

                            _ ->
                                []
                    else
                        []

                _ ->
                    []

        _ ->
            []


modeChanged : Bool -> Key -> Mode -> Bool -> Buffer -> Buffer
modeChanged replaying key oldMode lineDeltaMotion buf =
    case buf.mode of
        Normal _ ->
            let
                ( y, x ) =
                    buf.cursor

                cursor =
                    case oldMode of
                        Insert data ->
                            ( y, max (x - 1) 0 )

                        _ ->
                            ( y
                            , if B.getLineMaxColumn y buf.lines > x then
                                x
                              else
                                max (x - 1) 0
                            )

                insert buf_ =
                    Buf.transaction
                        (repeatInserts oldMode buf_)
                        buf_

                changeColumn =
                    if lineDeltaMotion then
                        False
                    else
                        cursor /= buf.cursor
            in
                buf
                    |> Buf.setCursor cursor changeColumn
                    |> Buf.cancelLastIndent
                    |> insert
                    |> Buf.commit

        TempNormal ->
            buf
                |> Buf.commit

        Ex ({ prefix, exbuf } as ex) ->
            if B.isEmpty exbuf.lines then
                buf
                    |> handleKeypress False "<escape>"
                    |> Tuple.first
            else
                let
                    last =
                        buf.last

                    prefix1 =
                        case prefix of
                            ExSearch ({ forward } as search) ->
                                ExSearch <|
                                    case
                                        exbuf.lines
                                            |> B.toString
                                            |> String.dropLeft 1
                                            |> regexWith
                                                { caseInsensitive = True
                                                , multiline = False
                                                }
                                    of
                                        Just re ->
                                            { search
                                                | match =
                                                    matchString forward
                                                        re
                                                        buf.cursor
                                                        buf.lines
                                                , highlights =
                                                    matchAllStrings
                                                        re
                                                        scrollFrom
                                                        (scrollFrom + buf.view.size.height)
                                                        buf.lines
                                            }

                                        _ ->
                                            { search | match = Nothing, highlights = [] }

                            _ ->
                                prefix

                    buf1 =
                        if replaying || key == "<exbuf>" then
                            buf
                        else
                            case oldMode of
                                Ex _ ->
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

        (Insert { visual }) as data ->
            let
                last =
                    buf.last

                inserts =
                    if replaying || key == "<inserts>" then
                        last.inserts
                    else
                        case oldMode of
                            Insert _ ->
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
                        case oldMode of
                            Visual _ ->
                                last.visual ++ key

                            _ ->
                                ""
            in
                { buf | last = { last | visual = visual } }


setContinuation : String -> Buffer -> Buffer
setContinuation s buf =
    { buf | continuation = s }


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


scroll : Maybe Int -> V.ScrollPosition -> Buffer -> Buffer
scroll count value buf =
    -- TODO: scroll in visual mode
    let
        scope n =
            n
                |> max 0
                |> min (B.count buf.lines - 1)

        setCursor buf_ =
            case count of
                Just n ->
                    case value of
                        V.ScrollBy _ ->
                            buf_

                        _ ->
                            Buf.setCursor
                                ( scope (n - 1)
                                , Tuple.second buf_.cursor
                                )
                                False
                                buf_

                _ ->
                    buf_

        scrollInner buf_ =
            let
                y =
                    Tuple.first buf_.cursor

                y1 =
                    case value of
                        V.ScrollBy n ->
                            count
                                |> Maybe.withDefault 1
                                |> ((*) n)
                                |> ((+) buf_.view.scrollTop)

                        V.ScrollToTop ->
                            y

                        V.ScrollToBottom ->
                            y - buf_.view.size.height - 2 + 1

                        V.ScrollToMiddle ->
                            y - (buf_.view.size.height - 2) // 2
            in
                Buf.setScrollTop (scope y1) buf_
    in
        buf
            |> setCursor
            |> scrollInner


runOperator : Maybe Int -> String -> Operator -> Buffer -> ( Buffer, Cmd Msg )
runOperator count register operator buf =
    case operator of
        Move md mo ->
            case md of
                V.MatchChar _ _ ->
                    buf
                        |> motion count md mo
                        |> Tuple.mapFirst
                            (updateIme (\ime -> { ime | isActive = False }))

                _ ->
                    motion count md mo buf

        Select textobj around ->
            ( select count textobj around buf, Cmd.none )

        Scroll value ->
            buf
                |> scroll count value
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

        V.Undo ->
            ( Buf.undo buf, Cmd.none )

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
            ( jumpByView factor buf, Cmd.none )

        Put forward ->
            ( put register forward buf, Cmd.none )

        RepeatLastOperator ->
            replayKeys buf.dotRegister buf

        RepeatLastVisual ->
            replayKeys buf.last.visual buf

        RepeatLastEx ->
            replayKeys buf.last.ex buf

        VisualSwitchEnd ->
            ( Buf.switchVisualEnd buf, Cmd.none )

        Execute ->
            case buf.mode of
                Ex ex ->
                    ex.exbuf.lines
                        |> B.toString
                        |> String.dropLeft 1
                        |> (\s ->
                                execute count
                                    register
                                    s
                                    { buf
                                        | mode =
                                            Ex
                                                { ex
                                                    | exbuf =
                                                        clearExBufAutoComplete
                                                            ex.exbuf
                                                }
                                    }
                           )

                _ ->
                    ( buf, Cmd.none )

        Join collapseSpaces ->
            ( join count collapseSpaces buf, Cmd.none )

        Replace ch ->
            ( buf
                |> applyReplace count ch
                |> updateIme (\ime -> { ime | isActive = False })
            , Cmd.none
            )

        ToggleTip ->
            ( Buf.setShowTip (not buf.view.showTip) buf, Cmd.none )

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
            jumpHistory isForward buf

        JumpLastBuffer ->
            jumpLastBuffer buf

        JumpToTag ->
            startJumpToTag count buf

        JumpBackFromTag ->
            case buf.last.jumpToTag of
                Just loc ->
                    jumpToLocation True loc buf

                _ ->
                    ( buf, Cmd.none )

        JumpToFile ->
            jumpToFile buf

        Indent forward range ->
            ( applyIndent count forward range buf, Cmd.none )

        IncreaseNumber larger ->
            ( increaseNumber count larger buf, Cmd.none )

        CaseOperator changeCase range ->
            ( applyCaseOperator count changeCase range buf, Cmd.none )

        ColumnInsert append ->
            ( columnInsert append buf, Cmd.none )

        ShowInfo ->
            let
                n =
                    B.count buf.lines - 1

                ( y, x ) =
                    buf.cursor
            in
                ( Buf.infoMessage
                    ((buf |> Buf.shortPath |> quote)
                        ++ " line "
                        ++ (y
                                |> inc
                                |> String.fromInt
                           )
                        ++ " of "
                        ++ (B.count buf.lines |> String.fromInt)
                        ++ " --"
                        ++ (y * 100 // n |> String.fromInt)
                        ++ "%-- col "
                        ++ (x
                                |> inc
                                |> String.fromInt
                           )
                    )
                    buf
                , Cmd.none
                )

        IMEToggleActive ->
            ( updateIme
                (\ime -> { ime | isActive = not ime.isActive })
                buf
            , Cmd.none
            )

        _ ->
            ( buf, Cmd.none )


quote : String -> String
quote s =
    "\"" ++ s ++ "\""


columnInsert : Bool -> Buffer -> Buffer
columnInsert prepend buf =
    case buf.mode of
        Visual { tipe, begin, end } ->
            case tipe of
                V.VisualBlock ->
                    let
                        ( by, bx ) =
                            begin

                        ( ey, ex ) =
                            end

                        minY =
                            min by ey

                        minX =
                            min bx ex
                    in
                        if prepend then
                            Buf.setCursor ( minY, minX ) True buf
                        else
                            let
                                x =
                                    max bx ex + 1

                                maxcol =
                                    B.getLineMaxColumn minY buf.lines

                                patches =
                                    if maxcol < x then
                                        -- fill spaces
                                        [ Insertion
                                            ( minY, maxcol )
                                            (B.fromString <|
                                                String.repeat (x - maxcol) " "
                                            )
                                        ]
                                    else
                                        []
                            in
                                buf
                                    |> Buf.transaction patches
                                    |> Buf.setCursor ( minY, x ) True

                _ ->
                    buf

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


isExMode mode =
    case mode of
        Ex _ ->
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
                parseKeys s

            ( buf1, cmd ) =
                List.foldl
                    (\key ( buf_, cmd_ ) ->
                        let
                            ( buf__, cmd1 ) =
                                handleKeypress True key buf_
                        in
                            ( buf__, Cmd.batch [ cmd_, cmd1 ] )
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
        |> (\msg -> update msg buf)


execute : Maybe Int -> String -> String -> Buffer -> ( Buffer, Cmd Msg )
execute count register str buf =
    case
        str
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
                    (path
                        |> normalizePath buf.config.pathSeperator
                        |> replaceHomeDir buf.config.homedir
                    )
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
            ( buf
            , cwd
                |> replaceHomeDir buf.config.homedir
                |> sendCd buf.config.service
            )

        [ s ] ->
            case String.toInt s of
                Just n ->
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


focusHiddenInput : Cmd Msg
focusHiddenInput =
    Task.attempt
        (always NoneMessage)
        (Dom.focus "hidden-input")


applyVimAST : Bool -> Key -> AST -> Buffer -> ( Buffer, Cmd Msg )
applyVimAST replaying key ast buf =
    let
        { count, edit, modeName, register, recordKeys } =
            ast

        -- |> Debug.log key
        oldMode =
            -- For now the put operator is implemented as
            --   1) Start insert mode
            --   2) Put string
            --   3) Back to normal mode
            if modeName == V.ModeNameNormal && isEnterInsertMode edit then
                Insert
                    { autoComplete = Nothing
                    , startCursor = buf.cursor
                    , visual = Nothing
                    }
            else
                buf.mode

        saveDotRegister replaying_ buf_ =
            if replaying_ then
                buf_
            else
                case recordKeys of
                    "" ->
                        buf_

                    s ->
                        { buf_ | dotRegister = s }

        lineDeltaMotion =
            edit
                |> Maybe.map isLineDeltaMotion
                |> Maybe.withDefault False

        setMatchedCursor oldBuf buf_ =
            if
                (pairSource oldBuf /= pairSource buf_)
                    || (oldBuf.view.scrollTop /= buf_.view.scrollTop)
                    || (oldBuf.view.size /= buf_.view.size)
                    || (oldBuf.lines /= buf_.lines)
                    || (oldBuf.syntax /= buf_.syntax)
            then
                pairCursor buf_
            else
                buf_

        doTokenize oldBuf ( buf_, cmds ) =
            if (oldBuf.path == buf_.path) || (Buf.isEditing oldBuf buf_) then
                ( buf_, (debounceTokenize 100) :: cmds )
            else
                ( buf_, cmds )

        doLint oldBuf ( buf_, cmds ) =
            if Buf.isEditing oldBuf buf_ then
                let
                    delay =
                        case buf_.mode of
                            Insert _ ->
                                500

                            _ ->
                                0
                in
                    ( buf_, debounceLint delay :: cmds )
            else
                ( buf_, cmds )

        doFocusInput oldBuf ( buf_, cmds ) =
            if
                (oldBuf.ime.isActive /= buf_.ime.isActive)
                    || (isExMode oldBuf.mode /= isExMode buf_.mode)
            then
                ( buf_, focusHiddenInput :: cmds )
            else
                ( buf_, cmds )
    in
        buf
            |> applyEdit count edit register
            |> Tuple.mapFirst
                (updateMode modeName
                    >> correctLines
                    >> modeChanged replaying key oldMode lineDeltaMotion
                    >> correctCursor
                    >> scrollToCursor
                    >> saveDotRegister replaying
                    >> setMatchedCursor buf
                )
            |> Tuple.mapSecond List.singleton
            |> doLint buf
            |> doTokenize buf
            |> doFocusInput buf
            |> Tuple.mapSecond Cmd.batch


onTokenized : Buffer -> Result error TokenizeResponse -> ( Buffer, Cmd Msg )
onTokenized buf resp =
    case resp of
        Ok payload ->
            case payload of
                TokenizeSuccess begin syntax ->
                    ( let
                        syntax1 =
                            buf.syntax
                                |> Array.slice 0 begin
                                |> (\s -> Array.append s syntax)

                        view =
                            buf.view

                        updateViewLineSyntax ({ lineNumber } as viewLine) =
                            if
                                (begin <= lineNumber)
                                    && (lineNumber < Array.length syntax1)
                            then
                                case Array.get lineNumber syntax1 of
                                    Just tokens ->
                                        { viewLine | tokens = tokens }

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


applyLintItems : List LintError -> Buffer -> Buffer
applyLintItems items buf =
    let
        normalizeFile file =
            if
                String.isEmpty file
                    || (String.endsWith buf.path file)
                    || String.endsWith
                        "912ec803b2ce49e4a541068d495ab570.elm"
                        file
            then
                buf.path
            else
                normalizePath buf.config.pathSeperator file

        normalizeRegion region =
            let
                ( b, e_ ) =
                    region

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

        items1 =
            List.map
                (\item ->
                    { item
                        | file = normalizeFile item.file
                        , region = normalizeRegion item.region
                    }
                )
                items
    in
        { buf
            | lint =
                { items = items1
                , count = List.length items1
                }
            , locationList =
                lintErrorToLocationList items1
        }


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


onMouseWheel : Int -> Buffer -> ( Buffer, Cmd Msg )
onMouseWheel delta buf =
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


update : Msg -> Buffer -> ( Buffer, Cmd Msg )
update message buf =
    case message of
        MouseWheel delta ->
            onMouseWheel delta buf

        PressKeys keys ->
            List.foldl
                (\key_ ( buf_, cmds ) ->
                    let
                        ( buf1, cmd ) =
                            handleKeypress False key_ buf_
                    in
                        ( buf1, cmd :: cmds )
                )
                ( buf, [] )
                (mapKeys buf.mode keys)
                |> Tuple.mapSecond (List.reverse >> Cmd.batch)

        Resize size ->
            onResize size buf

        ReadClipboard result ->
            case result of
                Ok { replaying, key, ast, s } ->
                    buf
                        |> Buf.setRegister "+" (Text s)
                        |> applyVimAST replaying key ast

                Err s ->
                    ( buf, Cmd.none )

        WriteClipboard _ ->
            ( buf, Cmd.none )

        Read result ->
            onRead result buf

        Write result ->
            onWrite result buf

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

        Lint id resp ->
            ( onLint id resp buf, Cmd.none )

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
            onReadTags result buf

        SearchResult result ->
            onSearch result buf

        ListFiles resp ->
            case resp of
                Ok files ->
                    ( listFiles files buf, Cmd.none )

                Err _ ->
                    ( buf, Cmd.none )

        SetCwd (Ok cwd) ->
            ( Buf.infoMessage cwd { buf | cwd = cwd }, Cmd.none )

        IMEMessage imeMsg ->
            case imeMsg of
                CompositionTry s ->
                    if buf.ime.isComposing then
                        ( buf, Cmd.none )
                    else
                        update (PressKeys s) buf

                CompositionWait s ->
                    ( buf
                    , Process.sleep 10
                        |> Task.attempt
                            (always (IMEMessage (CompositionTry s)))
                    )

                CompositionStart s ->
                    ( updateIme
                        (\ime ->
                            { ime
                                | isComposing = True
                                , compositionText = s
                            }
                        )
                        buf
                    , Cmd.none
                    )

                CompositionCommit keys ->
                    buf
                        |> update (PressKeys keys)
                        |> Tuple.mapSecond
                            (\cmd ->
                                Cmd.batch
                                    [ cmd
                                    , Process.sleep 10
                                        |> Task.attempt
                                            (always (IMEMessage CompositionEnd))
                                    ]
                            )

                CompositionEnd ->
                    ( updateIme
                        (\ime ->
                            { ime
                                | isComposing = False
                                , compositionText = ""
                            }
                        )
                        buf
                    , Cmd.none
                    )

                IMEFocus ->
                    ( buf, focusHiddenInput )

        NoneMessage ->
            ( buf, Cmd.none )

        Boot _ ->
            ( buf, Cmd.none )

        SetCwd _ ->
            ( buf, Cmd.none )


onLint : BufferIdentifier -> Result a (List LintError) -> Buffer -> Buffer
onLint ( path, version ) resp buf =
    if
        (path == buf.path)
            && (version == buf.history.version)
    then
        case resp of
            Ok items ->
                applyLintItems items buf

            _ ->
                buf
    else
        buf


onSearch : Result a String -> Buffer -> ( Buffer, Cmd Msg )
onSearch result buf =
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


onReadTags : Result String Location -> Buffer -> ( Buffer, Cmd Msg )
onReadTags result buf =
    case result of
        Ok loc ->
            let
                last =
                    buf.last

                saveLastJumpToTag buf_ =
                    { buf_
                        | last =
                            { last
                                | jumpToTag =
                                    Just
                                        { path = buf_.path
                                        , cursor = buf_.cursor
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


onResize : Size -> Buffer -> ( Buffer, Cmd Msg )
onResize size buf =
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


onRead : Result Http.Error BufferInfo -> Buffer -> ( Buffer, Cmd Msg )
onRead result buf =
    case result of
        Ok info ->
            editBuffer info buf

        Err (Http.BadStatus resp) ->
            case resp.status.code of
                404 ->
                    jumpTo True
                        { emptyBufferInfo
                            | path = resp.body
                            , content =
                                Just
                                    ( B.fromString B.lineBreak
                                    , Array.empty
                                    )
                        }
                        buf

                _ ->
                    ( buf, Cmd.none )

        _ ->
            ( Buf.errorMessage
                ("read " ++ Buf.shortPath buf ++ " failed")
                buf
            , Cmd.none
            )


onWrite : Result a (List Patch) -> Buffer -> ( Buffer, Cmd Msg )
onWrite result buf =
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
                            ((buf |> Buf.shortPath |> quote) ++ " Written")

                syntaxBottom =
                    buf.syntaxDirtyFrom

                lintCmd =
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
                    [ lintCmd
                    , tokenizeBufferCmd buf1
                    ]
                )

        _ ->
            ( Buf.errorMessage
                (Buf.shortPath buf ++ " save error")
                buf
            , Cmd.none
            )


listFiles : List String -> Buffer -> Buffer
listFiles files buf =
    let
        fileNameWordChars =
            "/\\-._"
    in
        case buf.mode of
            Ex ({ exbuf } as ex) ->
                setExbuf buf
                    ex
                    (case autoCompleteTarget fileNameWordChars exbuf of
                        Just ( pos, word ) ->
                            startAutoComplete fileNameWordChars
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

            _ ->
                buf


initCommand : Flags -> Cmd Msg
initCommand =
    sendBoot


getViewHeight : Int -> Int -> Int -> Int
getViewHeight heightPx lineHeightPx statusBarHeight =
    heightPx // lineHeightPx - statusBarHeight


init : Flags -> ( Buffer, Cmd Msg )
init flags =
    let
        { cwd, fontInfo, service, buffers, homedir } =
            flags

        --|> Debug.log "flags"
        lineHeight =
            fontInfo.lineHeight

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
                            , fontInfo = fontInfo
                            , homedir = homedir
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
        , Cmd.batch [ cmd, focusHiddenInput ]
        )
