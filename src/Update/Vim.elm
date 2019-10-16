module Update.Vim exposing
    ( applyDiff
    , applyEdit
    , applyVimAST
    , cmdNone
    , correctLines
    , editTestBuffer
    , execute
    , handleKeypress
    , initMode
    , isEnterInsertMode
    , modeChanged
    , replayKeys
    , resizeViews
    , runOperator
    , saveExHistory
    , serviceBeforeApplyVimAST
    , switchView
    , updateDebouncers
    , updateMode
    , updateScrollLeftPx
    , updateViewAfterCursorChanged
    )

import Clipboard
import Debouncers exposing (..)
import Dict
import Fs
import Helper.Helper exposing (..)
import Ime exposing (..)
import Internal.Jumps exposing (..)
import Internal.Syntax exposing (Syntax, fromTreeSitter)
import Internal.TextBuffer as B exposing (Patch(..))
import Internal.Window as Win
import Model exposing (..)
import Model.Buffer exposing (..)
import Model.Frame as Frame exposing (Frame)
import Model.Global exposing (..)
import Model.Size exposing (Size)
import Model.View as View exposing (..)
import TreeSitter as TS
import Update.AutoComplete exposing (..)
import Update.Buffer as Buf
import Update.CTag exposing (..)
import Update.CaseOperator exposing (applyCaseOperator)
import Update.Cursor exposing (..)
import Update.Delete exposing (..)
import Update.Increase exposing (increaseNumber)
import Update.Indent exposing (applyIndent)
import Update.Insert exposing (..)
import Update.Jump exposing (..)
import Update.Message exposing (..)
import Update.Motion exposing (..)
import Update.Replace exposing (applyReplace)
import Update.Select exposing (select)
import Update.Service exposing (..)
import Update.Tokenize exposing (..)
import Update.Yank exposing (put, yank, yankWholeBuffer)
import Vim.AST as V exposing (AST, Operator(..), isEditingOperator)
import Vim.Helper exposing (parseKeys)
import Vim.Parser exposing (parse)


initMode : Buffer -> V.ModeName -> Mode
initMode { view, mode } modeName =
    let
        cursor =
            view.cursor
    in
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
                { prefix = stringToPrefix prefix
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

        setCursor : Mode -> Buffer -> Buffer
        setCursor mode buf_ =
            if oldModeName == V.ModeNameVisual V.VisualBlock then
                case mode of
                    Insert { startCursor } ->
                        if startCursor /= buf_.view.cursor then
                            { buf_ | view = View.setCursor startCursor True buf_.view }

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
            if oldModeName /= newModeName then
                setImeActive (newModeName == V.ModeNameInsert) global.ime

            else
                global.ime

        viewLines =
            View.scrollViewLines
                view.size.height
                scrollFrom
                scrollTo
                view.lines
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
                        | lines = viewLines
                        , gutterLines =
                            if scrollFrom == scrollTo then
                                view.gutterLines

                            else
                                viewLines
                    }
            }
        , global = { global | ime = ime }
    }


modeChanged : Bool -> Key -> Mode -> Bool -> Editor -> Editor
modeChanged replaying key oldMode lineDeltaMotion ({ buf, global } as ed) =
    case buf.mode of
        Normal _ ->
            let
                ( y, x ) =
                    buf.view.cursor

                cursor =
                    case oldMode of
                        Insert _ ->
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
                        cursor /= buf.view.cursor
            in
            { ed
                | buf =
                    { buf | view = View.setCursor cursor changeColumn buf.view }
                        |> Buf.cancelLastIndent
                        |> insert
                        |> Buf.commit
            }

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
                                                        buf.view.cursor
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
                        { buf | mode = Ex { ex | prefix = prefix1 } }

                    scrollFrom =
                        Buf.finalScrollTop buf

                    scrollTo =
                        Buf.finalScrollTop buf1
                in
                { ed
                    | buf =
                        { buf1
                            | view =
                                let
                                    view =
                                        buf1.view

                                    viewLines =
                                        View.scrollViewLines
                                            view.size.height
                                            scrollFrom
                                            scrollTo
                                            view.lines
                                in
                                { view
                                    | lines = viewLines
                                    , gutterLines =
                                        if scrollFrom == scrollTo then
                                            view.gutterLines

                                        else
                                            viewLines
                                }
                        }
                    , global = global1
                }

        Insert _ ->
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
            [ Insertion buf.view.cursor <| B.fromString B.lineBreak ]
            buf

    else
        buf


editTestBuffer : String -> Editor -> ( Editor, Cmd Msg )
editTestBuffer path ({ global } as ed) =
    let
        ( global1, buf1 ) =
            createBuffer path global

        global2 =
            Buf.addBuffer buf1 global1
    in
    jumpToPath True path Nothing { ed | global = global2 }


execute : Maybe Int -> String -> String -> Editor -> ( Editor, Cmd Msg )
execute count register str { buf, global } =
    let
        ed =
            { buf = buf
            , global = { global | exHistory = saveExHistory str global.exHistory }
            }

        edit path =
            -- for unit testing
            if path == "*Test*" then
                editTestBuffer path ed

            else
                jumpToPath True path Nothing ed
    in
    case splitFirstSpace str of
        ( "e", path ) ->
            edit path

        ( "o", path ) ->
            edit path

        ( "b", path ) ->
            edit path

        ( "w", "" ) ->
            ( ed, sendWriteBuffer global.fs buf.path buf )

        ( "ll", "" ) ->
            let
                n =
                    Maybe.withDefault 1 count - 1
            in
            case nthList n global.locationList of
                Just loc ->
                    jumpToLocation True loc ed

                _ ->
                    ( { ed | buf = Buf.errorMessage "location not found" buf }
                    , Cmd.none
                    )

        ( "f", s ) ->
            ( ed, sendSearch global.fs s )

        ( "copy", "" ) ->
            yankWholeBuffer ed

        ( "cd", "" ) ->
            ( updateBuffer (Buf.infoMessage <| Fs.workingDir global.fs) ed
            , Cmd.none
            )

        ( "cd", cwd ) ->
            ( ed
            , cwd
                |> Fs.absolutePath global.fs
                |> sendCd global.fs
            )

        ( "mkdir", "" ) ->
            ( { ed | buf = Buf.errorMessage "Argument Error: mkdir path" buf }
            , Cmd.none
            )

        ( "mkdir", path ) ->
            ( ed
            , path
                |> Fs.absolutePath global.fs
                |> sendMkDir global.fs
            )

        ( "vsp", "" ) ->
            ( { ed
                | global =
                    { global
                        | window =
                            global.window
                                |> Win.getActiveFrame
                                |> Maybe.map
                                    (\frame ->
                                        Win.vsplit 0.5 frame global.window
                                    )
                                |> Maybe.map
                                    (resizeViews global.size global.lineHeight)
                                |> Maybe.withDefault global.window
                    }
              }
            , Cmd.none
            )

        ( "hsp", "" ) ->
            ( { ed
                | global =
                    { global
                        | window =
                            global.window
                                |> Win.getActiveFrame
                                |> Maybe.map
                                    (\frame ->
                                        Win.hsplit 0.5 frame global.window
                                    )
                                |> Maybe.map
                                    (resizeViews global.size global.lineHeight)
                                |> Maybe.withDefault global.window
                    }
              }
            , Cmd.none
            )

        ( "q", "" ) ->
            ( { ed
                | global =
                    { global
                        | window =
                            Win.removeCurrent global.window
                                |> resizeViews global.size global.lineHeight
                    }
                        |> cleanBuffers
              }
            , Cmd.none
            )

        ( "on", "" ) ->
            ( ed, Cmd.none )

        ( s, "" ) ->
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


saveExHistory : String -> List String -> List String
saveExHistory s exHistory =
    case exHistory of
        h :: _ ->
            if s == h then
                exHistory

            else
                (s :: exHistory)
                    |> List.take 50

        _ ->
            [ s ]


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
                Put _ ->
                    if ast.register == "+" then
                        Clipboard.read service
                            |> Cmd.map
                                (Result.map
                                    (\s ->
                                        { replaying = replaying
                                        , key = key
                                        , ast = ast
                                        , s = s
                                        }
                                    )
                                    >> ReadClipboard
                                )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


replayKeys : String -> Editor -> ( Editor, Cmd Msg )
replayKeys s ({ global } as ed) =
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
                                        updateIme (setImeActive False)
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
                    let
                        view =
                            buf.view
                                |> scroll count value (B.count buf.lines) global
                                |> cursorScope global.lineHeight buf.lines
                    in
                    { buf | view = view }
                        |> setVisualEnd view.cursor
            }
                |> cmdNone

        InsertString s ->
            case s of
                V.LastSavedString ->
                    replayKeys global.last.inserts ed

                _ ->
                    ( { ed | buf = insert s buf }, Cmd.none )

        Delete rg ->
            ed
                |> delete count register rg
                |> cmdNone

        Yank rg ->
            if register == "#" || register == "%" then
                ( ed, Cmd.none )

            else
                yank count register rg ed

        V.Undo ->
            let
                global1 =
                    updateJumps
                        (saveJump { path = buf.path, cursor = buf.view.cursor })
                        global

                buf1 =
                    Buf.undo buf
            in
            ( { ed | global = global1, buf = buf1 }, Cmd.none )

        Redo ->
            let
                global1 =
                    updateJumps
                        (saveJump { path = buf.path, cursor = buf.view.cursor })
                        global

                buf1 =
                    buf
                        |> Buf.redo
                        |> Buf.indentCursorToLineFirst
            in
            ( { ed | global = global1, buf = buf1 }, Cmd.none )

        OpenNewLine forward ->
            ed
                |> updateBuffer
                    (openNewLine
                        (if forward then
                            Tuple.first buf.view.cursor + 1

                         else
                            Tuple.first buf.view.cursor
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
                    execute count
                        register
                        (ex.exbuf.lines |> B.toString |> String.dropLeft 1)
                        ed

                _ ->
                    ( ed, Cmd.none )

        Join collapseSpaces ->
            ( updateBuffer (join count collapseSpaces) ed, Cmd.none )

        Replace ch ->
            ( ed
                |> updateBuffer (applyReplace count ch global)
                |> updateGlobal (updateIme (setImeActive False))
            , Cmd.none
            )

        ToggleTip ->
            ( updateGlobal (Buf.setShowTip (not global.showTip)) ed, Cmd.none )

        SelectAutoComplete forward ->
            ( updateBuffer (handleSelectWord forward) ed, Cmd.none )

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
            ( { ed | buf = applyIndent count forward range global buf }, Cmd.none )

        IncreaseNumber larger ->
            ( { ed | buf = increaseNumber count larger buf }, Cmd.none )

        CaseOperator changeCase range ->
            ( { ed | buf = applyCaseOperator count changeCase range global buf }, Cmd.none )

        ColumnInsert append ->
            ( { ed | buf = columnInsert append buf }, Cmd.none )

        ShowInfo ->
            ( { ed | buf = Buf.infoMessage (Buf.bufferInfo global buf) buf }, Cmd.none )

        IMEToggleActive ->
            ( { ed | global = updateIme toggleIme global }, Cmd.none )

        SelectHistory forward ->
            ( updateBuffer (handleSelectHistory forward global.exHistory) ed, Cmd.none )

        SwitchView type_ ->
            ( switchView type_ ed, Cmd.none )

        _ ->
            ( ed, Cmd.none )


switchView : V.SwitchViewType -> Editor -> Editor
switchView type_ ({ global } as ed) =
    let
        switch =
            case type_ of
                V.SwitchToNext ->
                    Win.activeNextFrame

                V.SwitchToPrev ->
                    Win.activePrevFrame

                V.SwitchToRight ->
                    Win.activeRightFrame

                V.SwitchToLeft ->
                    Win.activeLeftFrame

                V.SwitchToBottom ->
                    Win.activeBottomFrame

                V.SwitchToTop ->
                    Win.activeTopFrame
    in
    { ed
        | global =
            { global | window = switch global.window }
    }


applyEdit : Maybe Int -> Maybe Operator -> String -> Editor -> ( Editor, Cmd Msg )
applyEdit count edit register ed =
    case edit of
        Just operator ->
            let
                res =
                    runOperator count register operator ed
            in
            if isEditingOperator operator then
                let
                    ( ed1, cmd ) =
                        res

                    ( buf1, cmd1 ) =
                        updateAutoCompleteEdit ed1.global ed1.buf
                in
                ( { ed1 | buf = buf1 }, Cmd.batch [ cmd, cmd1 ] )

            else
                res

        Nothing ->
            ( ed, Cmd.none )


updateDebouncers debouncers ({ global } as ed) =
    { ed | global = { global | debouncers = debouncers } }


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


applyVimAST : Bool -> Key -> AST -> Editor -> ( Editor, Cmd Msg )
applyVimAST replaying key ast ({ buf } as ed) =
    let
        { count, edit, modeName, register, recordKeys } =
            ast

        oldMode =
            -- For now the put operator is implemented as
            --   1) Start insert mode
            --   2) Put string
            --   3) Back to normal mode
            if modeName == V.ModeNameNormal && isEnterInsertMode edit then
                Insert
                    { autoComplete = Nothing
                    , startCursor = buf.view.cursor
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
                |> Maybe.map V.isLineDeltaMotion
                |> Maybe.withDefault False

        doTokenize oldBuf ( ed_, cmds ) =
            if ed_.buf.config.syntax then
                if Buf.isEditing oldBuf ed_.buf then
                    ( ed_
                    , tokenizeLineCmd
                        ed_.global.service
                        ed_.buf.syntaxDirtyFrom
                        ed_.buf
                        :: cmds
                    )

                else
                    let
                        ( debouncers, cmd ) =
                            debounceTokenize Debouncing ed_.global.debouncers 500
                    in
                    ( updateDebouncers debouncers ed_, cmd :: cmds )

            else
                ( ed_, cmds )

        doLint oldBuf ( ed_, cmds ) =
            if Buf.isEditing oldBuf ed_.buf then
                case ed_.buf.mode of
                    Insert _ ->
                        let
                            ( debouncers, cmd ) =
                                debounceLint Debouncing ed_.global.debouncers 500
                        in
                        ( updateDebouncers debouncers ed_
                        , cmd :: cmds
                        )

                    _ ->
                        ( ed_, (toCmd <| Debounce SendLint) :: cmds )

            else
                ( ed_, cmds )

        doFocusIme oldEd ( ed_, cmds ) =
            if
                (isExMode oldEd.buf.mode /= isExMode ed_.buf.mode)
                    || (oldEd.global.ime /= ed_.global.ime)
            then
                ( ed_, Cmd.map IMEMessage focusIme :: cmds )

            else
                ( ed_, cmds )
    in
    ed
        |> applyEdit count edit register
        |> Tuple.mapFirst
            (updateMode modeName
                >> updateBuffer correctLines
                >> modeChanged replaying key oldMode lineDeltaMotion
                >> updateBuffer
                    (\buf1 ->
                        { buf1
                            | view =
                                updateViewAfterCursorChanged
                                    buf1.mode
                                    buf1.lines
                                    buf1.syntax
                                    buf1.view
                        }
                    )
                >> updateGlobal (saveDotRegister replaying)
                >> applyDiff
                >> updateScrollLeftPx
            )
        |> Tuple.mapSecond List.singleton
        |> doLint buf
        --|> doTokenize buf
        |> doFocusIme ed
        |> Tuple.mapSecond Cmd.batch


updateSyntax : Buffer -> View -> View
updateSyntax buf view =
    { view
        | syntax =
            fromTreeSitter
                view.scrollTop
                (view.scrollTop + view.size.height)
                buf.treeSitter.tree
    }


applyDiff : Editor -> Editor
applyDiff ed =
    if List.isEmpty ed.buf.history.diff then
        ed

    else
        let
            buf =
                ed.buf

            diff =
                buf.history.diff
                    |> List.reverse

            global1 =
                updateJumps
                    (applyPatchesToJumps buf.path diff)
                    ed.global

            { history, view } =
                buf

            lintItems =
                Buf.applyPatchesToLintErrors
                    buf.path
                    global1.lint.items
                    diff

            s =
                B.toString buf.lines

            buf1 =
                { buf
                    | treeSitter =
                        { parser = buf.treeSitter.parser
                        , tree =
                            TS.parse buf.treeSitter.parser
                                (\t arg ->
                                    case arg.endIndex of
                                        Just endIndex ->
                                            ( String.slice arg.startIndex endIndex s, t )

                                        _ ->
                                            ( String.dropLeft arg.startIndex s, t )
                                )
                                buf.treeSitter.tree
                        }
                }
        in
        { ed
            | buf =
                { buf1
                    | history = { history | diff = [] }
                    , syntaxDirtyFrom =
                        diff
                            |> List.map
                                (\item ->
                                    case item of
                                        B.RegionAdd ( ( m, _ ), _ ) ->
                                            m

                                        B.RegionRemove ( ( m, _ ), _ ) ->
                                            m
                                )
                            |> List.minimum
                            |> Maybe.map (min buf1.syntaxDirtyFrom)
                            |> Maybe.withDefault buf1.syntaxDirtyFrom
                    , view =
                        view
                            |> applyDiffToView diff
                            |> updateSyntax buf1
                }
            , global =
                { global1
                    | lint =
                        { items = lintItems
                        , count = List.length lintItems
                        }
                    , locationList =
                        applyPatchesToLocations
                            buf1.path
                            diff
                            global1.locationList
                }
        }


resizeViews : Size -> Int -> Win.Window Frame -> Win.Window Frame
resizeViews size lineHeight =
    Win.mapFrame
        (\frame ( widthPercent, heightPercent ) ->
            Frame.resize
                { width = ceiling <| toFloat size.width * widthPercent
                , height = (ceiling <| toFloat size.height * heightPercent) // lineHeight
                }
                frame
        )


updateViewAfterCursorChanged : Mode -> B.TextBuffer -> Syntax -> View -> View
updateViewAfterCursorChanged mode lines syntax =
    correctCursor (isExculdLineBreak mode) lines
        >> scrollToCursor
        >> pairCursor mode lines syntax


updateScrollLeftPx : Editor -> Editor
updateScrollLeftPx ({ global, buf } as ed) =
    { ed
        | buf =
            { buf
                | view =
                    let
                        view =
                            buf.view
                    in
                    { view | scrollLeftPx = getScrollLeftPx global.fontInfo buf }
            }
    }
