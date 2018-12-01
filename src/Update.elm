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
        , getLast
        , pathFileName
        , pathBase
        , resolvePath
        , relativePath
        )
import Vim.Parser exposing (parse)
import Vim.AST as V exposing (Operator(..))
import Update.Buffer as Buf
import Dict exposing (Dict)
import Update.Motion exposing (..)
import Update.Delete exposing (..)
import Update.Insert exposing (..)
import Regex as Re
import Update.Service exposing (..)
import Update.Yank exposing (yank, put, yankWholeBuffer)
import Helper.Debounce exposing (debounceLint, debounceTokenize)
import Array as Array exposing (Array)
import Internal.Jumps
    exposing
        ( Location
        , applyPatchesToJumps
        , applyPatchesToLocations
        )
import Internal.TextBuffer as B exposing (Patch(..))
import Internal.Position exposing (positionShiftLeft)
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


updateMode : V.ModeName -> Editor -> Editor
updateMode modeName ({ global, buf } as ed) =
    let
        oldModeName =
            getModeName buf.mode

        ( newMode, newModeName ) =
            if oldModeName == modeName then
                ( buf.mode, oldModeName )
            else if
                (oldModeName /= V.ModeNameInsert)
                    && (modeName == V.ModeNameInsert)
                    && buf.motionFailed
            then
                -- Don't change to insert mode when motion failed
                ( if oldModeName /= V.ModeNameNormal then
                    initMode buf V.ModeNameNormal
                  else
                    buf.mode
                , oldModeName
                )
            else
                ( initMode buf modeName, modeName )

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
            Buf.finalScrollTop global.size buf

        scrollTo =
            Buf.finalScrollTop global.size buf1

        ime =
            if oldModeName /= newModeName then
                let
                    oldIme =
                        global.ime
                in
                    case newModeName of
                        V.ModeNameInsert ->
                            { oldIme | isActive = True }

                        _ ->
                            { oldIme | isActive = False }
            else
                global.ime
    in
        { ed
            | buf =
                { buf1
                    | motionFailed = False
                    , continuation =
                        if buf.motionFailed then
                            ""
                        else
                            buf1.continuation
                    , view =
                        { view
                            | lines =
                                Buf.scrollViewLines
                                    global.size.height
                                    scrollFrom
                                    scrollTo
                                    view.lines
                        }
                }
            , global = { global | ime = ime }
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


modeChanged : Bool -> Key -> Mode -> Bool -> Editor -> Editor
modeChanged replaying key oldMode lineDeltaMotion ({ buf, global } as ed) =
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
                updateBuffer
                    (Buf.setCursor cursor changeColumn
                        >> Buf.cancelLastIndent
                        >> insert
                        >> Buf.commit
                    )
                    ed

        TempNormal ->
            updateBuffer Buf.commit ed

        Ex ({ prefix, exbuf } as ex) ->
            if B.isEmpty exbuf.lines then
                ed
                    |> handleKeypress False "<escape>"
                    |> Tuple.first
            else
                let
                    last =
                        global.last

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
                                                        (scrollFrom + global.size.height)
                                                        buf.lines
                                            }

                                        _ ->
                                            { search | match = Nothing, highlights = [] }

                            _ ->
                                prefix

                    global1 =
                        if replaying || key == "<exbuf>" then
                            global
                        else
                            case oldMode of
                                Ex _ ->
                                    { global
                                        | last = { last | ex = last.ex ++ key }
                                    }

                                _ ->
                                    { global
                                        | last = { last | ex = "" }
                                    }

                    buf1 =
                        Buf.setMode
                            (Ex { ex | prefix = prefix1 })
                            buf

                    scrollFrom =
                        Buf.finalScrollTop global.size buf

                    scrollTo =
                        Buf.finalScrollTop global.size buf1
                in
                    { ed
                        | buf =
                            Buf.updateView
                                (\view ->
                                    { view
                                        | lines =
                                            Buf.scrollViewLines
                                                global.size.height
                                                scrollFrom
                                                scrollTo
                                                view.lines
                                    }
                                )
                                buf1
                        , global = global1
                    }

        (Insert { visual }) as data ->
            let
                last =
                    global.last

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
                { ed
                    | global =
                        { global
                            | last = { last | inserts = inserts }
                        }
                }

        Visual _ ->
            let
                last =
                    global.last

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
                { ed
                    | global =
                        { global | last = { last | visual = visual } }
                }


cmdNone : a -> ( a, Cmd Msg )
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


scroll : Maybe Int -> V.ScrollPosition -> Global -> Buffer -> Buffer
scroll count value global buf =
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
                size =
                    global.size

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
                            y - size.height - 2 + 1

                        V.ScrollToMiddle ->
                            y - (size.height - 2) // 2
            in
                Buf.setScrollTop (scope y1) global buf_
    in
        buf
            |> setCursor
            |> scrollInner


runOperator : Maybe Int -> String -> Operator -> Editor -> ( Editor, Cmd Msg )
runOperator count register operator ({ buf, global } as ed) =
    case operator of
        Move md mo ->
            case md of
                V.MatchChar _ _ ->
                    ed
                        |> motion count md mo
                        |> Tuple.mapFirst
                            (\ed1 ->
                                { ed1
                                    | global =
                                        updateIme (\ime -> { ime | isActive = False })
                                            ed1.global
                                }
                            )

                _ ->
                    motion count md mo ed

        Select textobj around ->
            ( select count textobj around ed, Cmd.none )

        Scroll value ->
            { ed
                | buf =
                    buf
                        |> scroll count value global
                        |> cursorScope global
            }
                |> cmdNone

        InsertString s ->
            case s of
                V.LastSavedString ->
                    replayKeys global.last.inserts ed

                _ ->
                    { ed
                        | buf =
                            buf
                                |> insert s
                                |> filterAutoComplete
                    }
                        |> cmdNone

        Delete rg ->
            ed
                |> delete count register rg
                |> updateBuffer filterAutoComplete
                |> cmdNone

        Yank rg ->
            yank count register rg ed

        V.Undo ->
            ( updateBuffer Buf.undo ed, Cmd.none )

        Redo ->
            ed
                |> updateBuffer
                    (Buf.redo >> Buf.indentCursorToLineFirst)
                |> cmdNone

        OpenNewLine forward ->
            ed
                |> updateBuffer
                    (openNewLine
                        (if forward then
                            Tuple.first buf.cursor + 1
                         else
                            Tuple.first buf.cursor
                        )
                    )
                |> cmdNone

        JumpByView factor ->
            ( { ed | buf = jumpByView factor global buf }, Cmd.none )

        Put forward ->
            ( put register forward ed, Cmd.none )

        RepeatLastOperator ->
            replayKeys global.dotRegister ed

        RepeatLastVisual ->
            replayKeys global.last.visual ed

        RepeatLastEx ->
            replayKeys global.last.ex ed

        VisualSwitchEnd ->
            ( updateBuffer Buf.switchVisualEnd ed, Cmd.none )

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
                                    { ed
                                        | buf =
                                            { buf
                                                | mode =
                                                    Ex
                                                        { ex
                                                            | exbuf =
                                                                clearExBufAutoComplete
                                                                    ex.exbuf
                                                        }
                                            }
                                        , global =
                                            { global
                                                | exHistory =
                                                    (s :: global.exHistory)
                                                        |> List.take 50
                                            }
                                    }
                           )

                _ ->
                    ( ed, Cmd.none )

        Join collapseSpaces ->
            ( updateBuffer (join count collapseSpaces) ed, Cmd.none )

        Replace ch ->
            ( ed
                |> updateBuffer (applyReplace count ch global)
                |> updateGlobal (updateIme (\ime -> { ime | isActive = False }))
            , Cmd.none
            )

        ToggleTip ->
            ( updateGlobal (Buf.setShowTip (not global.showTip)) ed
            , Cmd.none
            )

        SelectAutoComplete forward ->
            case buf.mode of
                Ex ex ->
                    ( { ed
                        | buf =
                            ex.exbuf
                                |> selectAutoComplete forward
                                |> setExbuf buf ex
                      }
                    , Cmd.none
                    )

                Insert { autoComplete } ->
                    case autoComplete of
                        Just _ ->
                            ( updateBuffer (selectAutoComplete forward) ed
                            , Cmd.none
                            )

                        _ ->
                            ( { ed
                                | buf =
                                    case autoCompleteTarget buf.config.wordChars buf of
                                        Just ( pos, word ) ->
                                            let
                                                exclude =
                                                    wordStringUnderCursor
                                                        buf.config.wordChars
                                                        buf.lines
                                                        (positionShiftLeft buf.cursor)
                                                        |> Maybe.map Tuple.second
                                                        |> Maybe.withDefault ""

                                                words =
                                                    Buf.toWords exclude buf
                                            in
                                                if List.isEmpty words then
                                                    Buf.errorMessage "Pattern no found"
                                                        buf
                                                else
                                                    buf
                                                        |> startAutoComplete
                                                            buf.config.wordChars
                                                            ""
                                                            0
                                                            words
                                                            pos
                                                            word
                                                        |> selectAutoComplete forward

                                        _ ->
                                            buf
                              }
                            , Cmd.none
                            )

                _ ->
                    ( ed, Cmd.none )

        JumpHistory isForward ->
            jumpHistory isForward ed

        JumpLastBuffer ->
            jumpLastBuffer ed

        JumpToTag ->
            startJumpToTag count ed

        JumpBackFromTag ->
            case global.last.jumpToTag of
                Just loc ->
                    jumpToLocation True loc ed

                _ ->
                    ( ed, Cmd.none )

        JumpToFile ->
            jumpToFile ed

        Indent forward range ->
            ( { ed | buf = applyIndent count forward range global buf }
            , Cmd.none
            )

        IncreaseNumber larger ->
            ( { ed | buf = increaseNumber count larger buf }
            , Cmd.none
            )

        CaseOperator changeCase range ->
            ( { ed | buf = applyCaseOperator count changeCase range global buf }
            , Cmd.none
            )

        ColumnInsert append ->
            ( { ed | buf = columnInsert append buf }, Cmd.none )

        ShowInfo ->
            let
                n =
                    B.count buf.lines - 1

                ( y, x ) =
                    buf.cursor
            in
                ( updateBuffer
                    (Buf.infoMessage
                        ((buf |> Buf.shortPath global |> quote)
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
                    )
                    ed
                , Cmd.none
                )

        IMEToggleActive ->
            ( { ed
                | global =
                    updateIme
                        (\ime -> { ime | isActive = not ime.isActive })
                        global
              }
            , Cmd.none
            )

        SelectHistory forward ->
            case buf.mode of
                Ex ex ->
                    ( { ed
                        | buf =
                            if isAutoCompleteStarted ex.exbuf "$$%exHistory" then
                                ex.exbuf
                                    |> selectAutoComplete forward
                                    |> setExbuf buf ex
                            else
                                ex.exbuf
                                    |> startAutoComplete ""
                                        "$$%exHistory"
                                        1
                                        (List.reverse global.exHistory)
                                        ( 0, 1 )
                                        (B.toString ex.exbuf.lines
                                            |> String.dropLeft 1
                                        )
                                    |> selectAutoComplete forward
                                    |> setExbuf buf ex
                      }
                    , Cmd.none
                    )

                _ ->
                    ( ed, Cmd.none )

        _ ->
            ( ed, Cmd.none )


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


setExbuf : Buffer -> ExMode -> Buffer -> Buffer
setExbuf buf ex exbuf =
    { buf | mode = Ex { ex | exbuf = exbuf } }


applyEdit : Maybe Int -> Maybe Operator -> String -> Editor -> ( Editor, Cmd Msg )
applyEdit count edit register ({ buf } as ed) =
    case edit of
        Just operator ->
            case buf.mode of
                Ex ex ->
                    let
                        ( ed1, cmd ) =
                            runOperator count register operator ed

                        global =
                            ed1.global
                    in
                        case ed1.buf.mode of
                            Ex ({ exbuf } as newex) ->
                                let
                                    s =
                                        B.toString exbuf.lines

                                    trigger =
                                        if isAutoCompleteStarted exbuf "$$%exHistory" then
                                            "$$%exHistory"
                                        else if String.startsWith ":o " s then
                                            global.cwd
                                        else if String.startsWith ":b " s then
                                            global.cwd
                                        else
                                            s
                                                |> String.trim
                                                |> String.split " "
                                                |> getLast
                                                |> Maybe.withDefault ""
                                                |> getPath
                                                    global.pathSeperator
                                                    global.homedir
                                                    global.cwd

                                    ( getList, clearAutoComplete ) =
                                        if isAutoCompleteStarted exbuf "$$%exHistory" then
                                            ( \a b c -> Cmd.none, False )
                                        else if String.startsWith ":o " s then
                                            ( sendListAllFiles, False )
                                        else if String.startsWith ":b " s then
                                            ( \a b c ->
                                                Task.succeed
                                                    (global.buffers
                                                        |> Dict.keys
                                                        |> List.filter
                                                            (\path ->
                                                                path /= buf.path && path /= ""
                                                            )
                                                        |> List.map (relativePath global.pathSeperator global.cwd)
                                                    )
                                                    |> Task.perform ListBuffers
                                            , False
                                            )
                                        else if String.startsWith ":e " s then
                                            ( sendListFiles, False )
                                        else if String.startsWith ":cd " s then
                                            ( sendListDirectories, False )
                                        else
                                            ( \a b c -> Cmd.none, True )
                                in
                                    if clearAutoComplete then
                                        ( { ed1
                                            | buf =
                                                setExbuf ed1.buf
                                                    newex
                                                    (clearExBufAutoComplete exbuf)
                                          }
                                        , cmd
                                        )
                                    else if isExEditing operator then
                                        if isAutoCompleteStarted exbuf trigger then
                                            ( { ed1
                                                | buf =
                                                    exbuf
                                                        |> filterAutoComplete
                                                        |> setExbuf ed1.buf newex
                                              }
                                            , cmd
                                            )
                                        else
                                            ( ed1
                                            , Cmd.batch
                                                [ cmd
                                                , getList
                                                    global.service
                                                    global.pathSeperator
                                                    trigger
                                                ]
                                            )
                                    else
                                        ( ed1, cmd )

                            _ ->
                                ( ed1, cmd )

                _ ->
                    runOperator count register operator ed

        Nothing ->
            ( ed, Cmd.none )


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


replayKeys : String -> Editor -> ( Editor, Cmd Msg )
replayKeys s ({ buf, global } as ed) =
    if s == "" then
        ( ed, Cmd.none )
    else
        let
            savedLast =
                global.last

            savedRegisters =
                global.registers

            keys =
                parseKeys s

            ( ed1, cmd ) =
                List.foldl
                    (\key ( ed_, cmd_ ) ->
                        let
                            ( ed__, cmd1 ) =
                                handleKeypress True key ed_
                        in
                            ( ed__, Cmd.batch [ cmd_, cmd1 ] )
                    )
                    ( ed, Cmd.none )
                    keys
        in
            ( { ed1
                | global =
                    { global
                        | last = savedLast
                        , registers = savedRegisters
                    }
              }
            , cmd
            )


editTestBuffer : String -> Editor -> ( Editor, Cmd Msg )
editTestBuffer path ({ buf } as ed) =
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
        |> (\msg -> update msg ed)


execute : Maybe Int -> String -> String -> Editor -> ( Editor, Cmd Msg )
execute count register str ({ buf, global } as ed) =
    let
        edit path =
            -- for unit testing
            if path == "*Test*" then
                editTestBuffer path ed
            else
                jumpToPath
                    True
                    (path
                        |> normalizePath global.pathSeperator
                        |> replaceHomeDir global.homedir
                    )
                    Nothing
                    ed
    in
        case
            str
                |> String.trim
                |> String.split " "
        of
            [ "e", path ] ->
                edit path

            [ "o", path ] ->
                edit path

            [ "b", path ] ->
                edit path

            [ "w" ] ->
                ( ed, sendWriteBuffer global.service buf.path buf )

            [ "ll" ] ->
                let
                    n =
                        (Maybe.withDefault 1 count) - 1
                in
                    case nthList n global.locationList of
                        Just loc ->
                            jumpToLocation True loc ed

                        _ ->
                            ( { ed | buf = Buf.errorMessage "location not found" buf }
                            , Cmd.none
                            )

            [ "f", s ] ->
                ( ed, sendSearch global.service global.cwd s )

            [ "copy" ] ->
                yankWholeBuffer ed

            [ "cd" ] ->
                ( { ed | buf = Buf.infoMessage global.cwd buf }, Cmd.none )

            [ "cd", cwd ] ->
                ( ed
                , cwd
                    |> replaceHomeDir global.homedir
                    |> resolvePath
                        global.pathSeperator
                        global.cwd
                    |> sendCd global.service
                )

            [ "mkdir" ] ->
                ( { ed | buf = Buf.errorMessage "Argument Error: mkdir path" buf }
                , Cmd.none
                )

            [ "mkdir", path ] ->
                ( ed
                , path
                    |> replaceHomeDir global.homedir
                    |> resolvePath
                        global.pathSeperator
                        global.cwd
                    |> sendMkDir global.service
                )

            [ s ] ->
                case String.toInt s of
                    Just n ->
                        runOperator (Just n)
                            register
                            (V.Move V.BufferBottom V.emptyMotionOption)
                            ed

                    _ ->
                        ( ed, Cmd.none )

            _ ->
                ( ed, Cmd.none )


handleKeypress : Bool -> Key -> Editor -> ( Editor, Cmd Msg )
handleKeypress replaying key ({ buf, global } as ed) =
    let
        cacheKey =
            ( buf.continuation, key )

        (( ast, continuation ) as cacheVal) =
            case Dict.get cacheKey global.vimASTCache of
                Just resp ->
                    resp

                _ ->
                    parse buf.continuation key

        ed1 =
            { ed
                | buf =
                    { buf
                        | continuation = continuation
                    }
                , global = cacheVimAST cacheKey cacheVal global
            }
    in
        case serviceBeforeApplyVimAST replaying key ast global.service of
            Just cmd ->
                ( ed1, cmd )

            _ ->
                applyVimAST replaying key ast ed1


serviceBeforeApplyVimAST : Bool -> Key -> AST -> String -> Maybe (Cmd Msg)
serviceBeforeApplyVimAST replaying key ast service =
    case ast.edit of
        Just op ->
            case op of
                Put forward ->
                    if ast.register == "+" then
                        sendReadClipboard
                            replaying
                            key
                            service
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


applyVimAST : Bool -> Key -> AST -> Editor -> ( Editor, Cmd Msg )
applyVimAST replaying key ast ({ buf } as ed) =
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

        saveDotRegister replaying_ global_ =
            if replaying_ then
                global_
            else
                case recordKeys of
                    "" ->
                        global_

                    s ->
                        { global_ | dotRegister = s }

        lineDeltaMotion =
            edit
                |> Maybe.map isLineDeltaMotion
                |> Maybe.withDefault False

        setMatchedCursor : Editor -> Editor -> Editor
        setMatchedCursor oldEd ed_ =
            let
                oldBuf =
                    oldEd.buf

                buf_ =
                    ed_.buf
            in
                if
                    (pairSource oldBuf /= pairSource buf_)
                        || (oldBuf.view.scrollTop /= buf_.view.scrollTop)
                        || (oldEd.global.size /= ed_.global.size)
                        || (oldBuf.lines /= buf_.lines)
                        || (oldBuf.syntax /= buf_.syntax)
                then
                    updateBuffer (pairCursor ed_.global.size) ed_
                else
                    ed_

        doTokenize oldBuf ( ed_, cmds ) =
            if (oldBuf.path == ed_.buf.path) || (Buf.isEditing oldBuf ed_.buf) then
                ( ed_, (debounceTokenize 100) :: cmds )
            else
                ( ed_, cmds )

        doLint oldBuf ( ed_, cmds ) =
            if Buf.isEditing oldBuf ed_.buf then
                let
                    delay =
                        case ed_.buf.mode of
                            Insert _ ->
                                500

                            _ ->
                                0
                in
                    ( ed_, debounceLint delay :: cmds )
            else
                ( ed_, cmds )

        doFocusInput oldEd ( ed_, cmds ) =
            if
                (oldEd.global.ime.isActive /= ed_.global.ime.isActive)
                    || (isExMode oldEd.buf.mode /= isExMode ed_.buf.mode)
            then
                ( ed_, focusHiddenInput :: cmds )
            else
                ( ed_, cmds )

        shiftLocations : Editor -> Editor
        shiftLocations ed1 =
            let
                buf1 =
                    ed1.buf

                diff =
                    buf1.history.diff

                global1 =
                    ed1.global

                history =
                    buf1.history

                view =
                    buf1.view
            in
                if List.isEmpty diff then
                    ed1
                else
                    { ed1
                        | buf =
                            { buf1
                                | history = { history | diff = [] }
                                , view =
                                    { view
                                        | lines =
                                            Buf.applyDiffToView
                                                diff
                                                view.scrollTop
                                                global1.size.height
                                                view.lines
                                    }
                            }
                        , global =
                            { global1
                                | jumps = applyPatchesToJumps diff global1.jumps
                                , lint =
                                    { items =
                                        Buf.applyPatchesToLintErrors
                                            global1.lint.items
                                            diff

                                    --|> Debug.log "update lint.items"
                                    , count = global1.lint.count
                                    }
                                , locationList =
                                    applyPatchesToLocations
                                        global1.locationList
                                        diff
                            }
                    }
    in
        ed
            |> applyEdit count edit register
            |> Tuple.mapFirst
                (updateMode modeName
                    >> updateBuffer correctLines
                    >> modeChanged replaying key oldMode lineDeltaMotion
                    >> updateBuffer correctCursor
                    >> updateBuffer (scrollToCursor ed.global)
                    >> updateGlobal (saveDotRegister replaying)
                    >> setMatchedCursor ed
                    >> shiftLocations
                )
            |> Tuple.mapSecond List.singleton
            |> doLint buf
            |> doTokenize buf
            |> doFocusInput ed
            |> Tuple.mapSecond Cmd.batch


onTokenized : Editor -> Result error TokenizeResponse -> ( Editor, Cmd Msg )
onTokenized ({ buf, global } as ed) resp =
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
                      in
                        { ed
                            | buf =
                                { buf
                                    | syntax = syntax1
                                    , syntaxDirtyFrom = Array.length syntax1
                                }
                                    |> pairCursor global.size
                        }
                    , Cmd.none
                    )

                LineTokenizeSuccess begin tokens ->
                    tokenizeBuffer
                        { ed
                            | buf =
                                { buf
                                    | syntax = Array.set begin tokens buf.syntax
                                    , syntaxDirtyFrom = begin + 1
                                }
                        }

                TokenizeCacheMiss ->
                    tokenizeBuffer
                        { ed
                            | buf =
                                { buf
                                    | syntax = Array.empty
                                    , syntaxDirtyFrom = 0
                                }
                        }

                TokenizeError s ->
                    let
                        config =
                            buf.config
                    in
                        ( { ed
                            | buf =
                                { buf
                                    | syntax = Array.empty
                                    , syntaxDirtyFrom = 0
                                    , config = { config | syntax = False }
                                }
                          }
                        , Cmd.none
                        )

        Err _ ->
            ( ed, Cmd.none )


applyLintItems : List LintError -> Buffer -> Global -> Global
applyLintItems items buf global =
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
                normalizePath global.pathSeperator file

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
        { global
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


onMouseWheel : Int -> Editor -> ( Editor, Cmd Msg )
onMouseWheel delta ({ buf, global } as ed) =
    let
        view =
            buf.view

        lineHeight =
            global.lineHeight

        scrollTopPx =
            (view.scrollTopPx + delta)
                |> max 0
                |> min ((B.count buf.lines - 2) * lineHeight)

        scrollTopDelta =
            scrollTopPx // lineHeight - buf.view.scrollTop
    in
        { ed
            | buf =
                { buf
                    | view = { view | scrollTopPx = scrollTopPx }
                }
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


update : Msg -> Editor -> ( Editor, Cmd Msg )
update message ({ buf, global } as ed) =
    case message of
        MouseWheel delta ->
            onMouseWheel delta ed

        PressKeys keys ->
            List.foldl
                (\key_ ( ed_, cmds ) ->
                    let
                        ( ed__, cmd ) =
                            handleKeypress False key_ ed_
                    in
                        ( ed__, cmd :: cmds )
                )
                ( ed, [] )
                (mapKeys buf.mode keys)
                |> Tuple.mapSecond (List.reverse >> Cmd.batch)

        Resize size ->
            onResize size ed

        ReadClipboard result ->
            case result of
                Ok { replaying, key, ast, s } ->
                    { ed | global = Buf.setRegister "+" (Text s) global }
                        |> applyVimAST replaying key ast

                Err s ->
                    ( ed, Cmd.none )

        WriteClipboard _ ->
            ( ed, Cmd.none )

        Read result ->
            onRead result ed

        Write result ->
            onWrite result ed

        SendLint ->
            if buf.config.lint then
                ( ed
                , sendLintOnTheFly
                    global.service
                    global.pathSeperator
                    buf.path
                    buf.history.version
                    buf.lines
                )
            else
                ( ed, Cmd.none )

        Lint id resp ->
            ( updateGlobal (onLint id resp buf) ed, Cmd.none )

        Tokenized ( path, version ) resp ->
            if
                (path == buf.path)
                    && (version == buf.history.version)
            then
                onTokenized ed resp
            else
                ( ed, Cmd.none )

        SendTokenize ->
            ( ed, tokenizeBufferCmd ed )

        ReadTags result ->
            onReadTags result ed

        SearchResult result ->
            onSearch result ed

        ListAllFiles resp ->
            case resp of
                Ok files ->
                    ( updateBuffer (startExAutoComplete 2 global.cwd files) ed, Cmd.none )

                Err _ ->
                    ( ed, Cmd.none )

        ListDirectries resp ->
            case resp of
                Ok files ->
                    ( updateBuffer (listFiles files global) ed, Cmd.none )

                Err _ ->
                    ( ed, Cmd.none )

        ListFiles resp ->
            case resp of
                Ok files ->
                    ( updateBuffer (listFiles files global) ed, Cmd.none )

                Err _ ->
                    ( ed, Cmd.none )

        ListBuffers files ->
            ( updateBuffer (startExAutoComplete 2 global.cwd files) ed, Cmd.none )

        SetCwd (Ok cwd) ->
            ( { ed
                | buf = Buf.infoMessage cwd buf
                , global = { global | cwd = cwd }
              }
            , Cmd.none
            )

        IMEMessage imeMsg ->
            case imeMsg of
                CompositionTry s ->
                    if global.ime.isComposing && s /= "<escape>" then
                        ( ed, Cmd.none )
                    else
                        update (PressKeys s) ed

                CompositionWait s ->
                    ( ed
                    , Process.sleep 10
                        |> Task.attempt
                            (always (IMEMessage (CompositionTry s)))
                    )

                CompositionStart s ->
                    ( updateGlobal
                        (updateIme
                            (\ime ->
                                { ime
                                    | isComposing = True
                                    , compositionText = s
                                }
                            )
                        )
                        ed
                    , Cmd.none
                    )

                CompositionCommit keys ->
                    ed
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
                    ( updateGlobal
                        (updateIme
                            (\ime ->
                                { ime
                                    | isComposing = False
                                    , compositionText = ""
                                }
                            )
                        )
                        ed
                    , Cmd.none
                    )

                IMEFocus ->
                    ( ed, focusHiddenInput )

        NoneMessage ->
            ( ed, Cmd.none )

        Boot _ ->
            ( ed, Cmd.none )

        SetCwd _ ->
            ( ed, Cmd.none )

        MakeDir res ->
            case res of
                Ok _ ->
                    ( ed, Cmd.none )

                Err err ->
                    ( updateBuffer (Buf.errorMessage err) ed, Cmd.none )


onLint : BufferIdentifier -> Result a (List LintError) -> Buffer -> Global -> Global
onLint ( path, version ) resp buf global =
    if
        (path == buf.path)
            && (version == buf.history.version)
    then
        case resp of
            Ok items ->
                applyLintItems items buf global

            _ ->
                global
    else
        global


onSearch : Result a String -> Editor -> ( Editor, Cmd Msg )
onSearch result ed =
    case result of
        Ok s ->
            jumpTo True
                { emptyBufferInfo
                    | path = "[Search]"
                    , content = Just ( B.fromString s, Array.empty )
                    , syntax = False
                }
                ed

        _ ->
            ( ed, Cmd.none )


onReadTags : Result String Location -> Editor -> ( Editor, Cmd Msg )
onReadTags result ed =
    case result of
        Ok loc ->
            let
                saveLastJumpToTag ed_ =
                    let
                        global_ =
                            ed_.global

                        buf_ =
                            ed_.buf

                        last =
                            global_.last
                    in
                        { ed_
                            | global =
                                { global_
                                    | last =
                                        { last
                                            | jumpToTag =
                                                Just
                                                    { path = buf_.path
                                                    , cursor = buf_.cursor
                                                    }
                                        }
                                }
                        }
            in
                ed
                    |> updateBuffer Buf.clearMessage
                    |> saveLastJumpToTag
                    |> jumpToLocation True loc

        _ ->
            ( updateBuffer (Buf.errorMessage "tag not found") ed
            , Cmd.none
            )


onResize : Size -> Editor -> ( Editor, Cmd Msg )
onResize size ({ buf, global } as ed) =
    let
        newHeight =
            getViewHeight size.height
                global.lineHeight
                global.statusbarHeight
    in
        { ed
            | global =
                { global
                    | size = { width = size.width, height = newHeight }
                }
            , buf =
                buf
                    |> Buf.updateView
                        (\view ->
                            { view
                                | lines =
                                    List.range
                                        view.scrollTop
                                        (view.scrollTop + newHeight + 1)
                            }
                        )
                    |> cursorScope global
        }
            |> tokenizeBuffer


onRead : Result Http.Error BufferInfo -> Editor -> ( Editor, Cmd Msg )
onRead result ({ buf, global } as ed) =
    case result of
        Ok info ->
            editBuffer True info ed

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
                        ed

                _ ->
                    ( ed, Cmd.none )

        _ ->
            ( updateBuffer
                (Buf.errorMessage
                    ("read " ++ Buf.shortPath global buf ++ " failed")
                )
                ed
            , Cmd.none
            )


onWrite : Result a ( String, List Patch ) -> Editor -> ( Editor, Cmd Msg )
onWrite result ({ buf, global } as ed) =
    case result of
        Ok ( lastModified, patches ) ->
            let
                -- TODO: add an unified `patch` function for buffer
                buf1 =
                    buf
                        |> Buf.transaction patches
                        |> Buf.commit
                        |> Buf.updateHistory
                            (\his ->
                                { his
                                    | lastModified = lastModified
                                    , savePoint = 0
                                    , changes = []
                                }
                            )
                        -- keep cursor position
                        |> Buf.setCursor buf.cursor True
                        |> correctCursor
                        |> scrollToCursor global
                        |> pairCursor global.size
                        |> Buf.infoMessage
                            ((buf |> Buf.shortPath global |> quote) ++ " Written")

                syntaxBottom =
                    buf.syntaxDirtyFrom

                lintCmd =
                    if buf1.config.lint then
                        sendLintProject global.service
                            global.pathSeperator
                            buf1.path
                            buf1.history.version
                            buf1.lines
                    else
                        Cmd.none

                ed1 =
                    { ed | buf = buf1 }
            in
                ( ed1
                , Cmd.batch
                    [ lintCmd
                    , tokenizeBufferCmd ed1
                    ]
                )

        _ ->
            ( updateBuffer
                (Buf.errorMessage
                    (Buf.shortPath global buf ++ " save error")
                )
                ed
            , Cmd.none
            )


getPath : String -> String -> String -> String -> String
getPath sep homedir cwd s1 =
    let
        s =
            replaceHomeDir homedir s1

        base =
            if s == homedir then
                s
            else
                pathBase sep s
    in
        resolvePath sep cwd base


fileNameWordChars : String
fileNameWordChars =
    "/\\-._"


startExAutoComplete : Int -> String -> List String -> Buffer -> Buffer
startExAutoComplete offset trigger candidates buf =
    case buf.mode of
        Ex ({ exbuf } as ex) ->
            setExbuf buf
                ex
                (startAutoComplete
                    fileNameWordChars
                    trigger
                    offset
                    candidates
                    ( 0, offset + 1 )
                    ""
                    exbuf
                )

        _ ->
            buf


listFiles : List String -> Global -> Buffer -> Buffer
listFiles files global buf =
    case buf.mode of
        Ex ({ exbuf } as ex) ->
            let
                sep =
                    global.pathSeperator
            in
                setExbuf buf
                    ex
                    (case
                        exbuf.lines
                            |> B.toString
                            |> String.split " "
                     of
                        [ prefix, path ] ->
                            startAutoComplete
                                fileNameWordChars
                                (getPath sep global.homedir global.cwd path)
                                (String.length prefix)
                                files
                                ( 0
                                , ((path
                                        |> pathBase sep
                                        |> String.length
                                   )
                                    + String.length prefix
                                    + String.length sep
                                  )
                                )
                                ""
                                exbuf

                        _ ->
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


init : Flags -> ( Editor, Cmd Msg )
init flags =
    let
        { cwd, fontInfo, service, buffers, homedir, isSafari } =
            flags

        --|> Debug.log "flags"
        lineHeight =
            fontInfo.lineHeight

        { activeBuffer, registers, height, pathSeperator, exHistory } =
            flags

        viewHeight =
            getViewHeight height lineHeight emptyGlobal.statusbarHeight

        cwd1 =
            if String.isEmpty cwd then
                "."
            else
                cwd

        activeBuf =
            activeBuffer
                |> Decode.decodeValue bufferInfoDecoder
                |> Result.withDefault emptyBufferInfo

        ( buf, cmd ) =
            editBuffer False
                activeBuf
                { buf =
                    { emptyBuffer
                        | view = emptyView
                        , path = activeBuf.path
                        , history = activeBuf.history
                    }
                , global =
                    { emptyGlobal
                        | service = service
                        , size =
                            { width = 1
                            , height = viewHeight
                            }
                        , exHistory = exHistory
                        , cwd = cwd1
                        , pathSeperator = pathSeperator
                        , fontInfo = fontInfo
                        , homedir = homedir
                        , isSafari = isSafari
                        , registers =
                            Decode.decodeValue registersDecoder registers
                                |> Result.withDefault Dict.empty
                        , buffers =
                            buffers
                                |> Decode.decodeValue (Decode.list bufferInfoDecoder)
                                |> Result.withDefault []
                                |> fromListBy .path
                        , lineHeight = lineHeight
                    }
                }
    in
        ( buf, Cmd.batch [ cmd, focusHiddenInput ] )
