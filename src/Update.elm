module Update exposing (applyDiff, init, initMode, update, updateActiveBuffer)

import Array as Array exposing (Array)
import Browser.Dom as Dom
import Debouncers exposing (..)
import Dict exposing (Dict)
import Font exposing (FontInfo, charWidth)
import Helper.Debounce as Deb
import Helper.Helper
    exposing
        ( fileNameWordChars
        , findFirst
        , fromListBy
        , getLast
        , httpErrorMessage
        , inc
        , isSpace
        , normalizePath
        , notSpace
        , nthList
        , pathBase
        , pathFileName
        , rangeCount
        , regexWith
        , replaceHomeDir
        , resolvePath
        , toAbsolutePath
        , toCmd
        )
import Http
import Ime exposing (emptyIme, focusIme, isImeActive, setImeActive)
import Internal.Jumps
    exposing
        ( Location
        , applyPatchesToJumps
        , applyPatchesToLocations
        )
import Internal.Position exposing (positionShiftLeft)
import Internal.Syntax exposing (Syntax)
import Internal.TextBuffer as B exposing (Patch(..))
import Internal.Window as Win
import Json.Decode as Decode
import Model exposing (..)
import Model.Buffer exposing (..)
import Model.Frame as Frame exposing (Frame, emptyFrame)
import Model.Global exposing (..)
import Model.Lint exposing (..)
import Model.LoadBuffer exposing (..)
import Model.Size exposing (Size)
import Model.View as View exposing (..)
import Parser as P exposing ((|.), (|=), Parser)
import Process
import Regex as Re exposing (Regex)
import Set
import Task
import Update.AutoComplete exposing (..)
import Update.Buffer as Buf
import Update.CaseOperator exposing (applyCaseOperator)
import Update.Cursor exposing (..)
import Update.Delete exposing (..)
import Update.Increase exposing (increaseNumber)
import Update.Indent exposing (applyIndent)
import Update.Insert exposing (..)
import Update.Jump exposing (..)
import Update.Keymap exposing (mapKeys)
import Update.Message exposing (..)
import Update.Motion exposing (..)
import Update.Range exposing (visualRegions)
import Update.Replace exposing (applyReplace)
import Update.Select exposing (select)
import Update.Service exposing (..)
import Update.Yank exposing (put, yank, yankWholeBuffer)
import Vim.AST as V exposing (AST, Operator(..), isEditingOperator)
import Vim.Helper exposing (escapeKey, parseKeys)
import Vim.Parser exposing (parse)


tokenizeLineCmd : String -> Int -> Buffer -> Cmd Msg
tokenizeLineCmd url begin buf =
    if buf.config.syntax then
        let
            view =
                buf.view
        in
        case B.getLine begin buf.lines of
            Just line ->
                sendTokenizeLine url
                    { bufId = buf.id
                    , path = buf.path
                    , version = buf.history.version
                    , line = begin
                    , lines = line
                    }

            _ ->
                Cmd.none

    else
        Cmd.none


tokenizeBufferCmd : Int -> String -> Buffer -> Cmd Msg
tokenizeBufferCmd begin url buf =
    if buf.config.syntax then
        let
            view =
                buf.view

            scrollTop =
                Buf.finalScrollTop view.size view buf

            scrollBottom =
                scrollTop + view.size.height

            end =
                scrollBottom + view.size.height
        in
        if begin < scrollBottom then
            let
                lines =
                    buf.lines
                        |> B.sliceLines begin end
                        |> B.toString
            in
            if String.isEmpty lines then
                Cmd.none

            else
                sendTokenize
                    url
                    { bufId = buf.id
                    , path = buf.path
                    , version = buf.history.version
                    , line = begin
                    , lines = lines
                    }

        else
            Cmd.none

    else
        Cmd.none


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

        setCursor : Mode -> Buffer -> Buffer
        setCursor mode buf_ =
            if oldModeName == V.ModeNameVisual V.VisualBlock then
                case mode of
                    Insert { startCursor } ->
                        if startCursor /= buf_.view.cursor then
                            Buf.updateView (View.setCursor startCursor True) buf_

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
            Buf.finalScrollTop buf.view.size buf.view buf

        scrollTo =
            Buf.finalScrollTop view.size view buf1

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
                        (startCursor <= buf.view.cursor)
                            && (Tuple.first startCursor == Tuple.first buf.view.cursor)
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
                                        B.sliceRegion startCursor
                                            ( Tuple.first buf.view.cursor
                                            , Tuple.second buf.view.cursor + 1
                                            )
                                            buf.lines

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
                    buf.view.cursor

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
                        cursor /= buf.view.cursor
            in
            updateBuffer
                (Buf.updateView (View.setCursor cursor changeColumn)
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
                        Buf.setMode
                            (Ex { ex | prefix = prefix1 })
                            buf

                    scrollFrom =
                        Buf.finalScrollTop buf.view.size buf.view buf

                    scrollTo =
                        Buf.finalScrollTop buf1.view.size buf1.view buf1
                in
                { ed
                    | buf =
                        Buf.updateView
                            (\view ->
                                let
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
                            )
                            buf1
                    , global = global1
                }

        Insert { visual } ->
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
            [ Insertion buf.view.cursor <|
                B.fromString B.lineBreak
            ]
            buf

    else
        buf


scroll : Maybe Int -> V.ScrollPosition -> Int -> Global -> View -> View
scroll count value lineCounts global view =
    let
        scope n =
            n
                |> max 0
                |> min (lineCounts - 1)

        setCursor : View -> View
        setCursor view_ =
            case count of
                Just n ->
                    case value of
                        V.ScrollBy _ ->
                            view_

                        _ ->
                            View.setCursor
                                ( scope (n - 1)
                                , Tuple.second view_.cursor
                                )
                                False
                                view_

                _ ->
                    view_

        scrollInner : View -> View
        scrollInner view_ =
            let
                size =
                    view_.size

                y =
                    Tuple.first view_.cursor

                y1 =
                    case value of
                        V.ScrollBy n ->
                            count
                                |> Maybe.withDefault 1
                                |> (*) n
                                |> (+) view_.scrollTop

                        V.ScrollToTop ->
                            y

                        V.ScrollToBottom ->
                            y - size.height - 2 + 1

                        V.ScrollToMiddle ->
                            y - (size.height - 2) // 2
            in
            View.setScrollTop (scope y1) global.lineHeight view_
    in
    view
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
                    buf
                        |> Buf.updateView (always view)
                        |> setVisualEnd view.cursor
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
                    }
                        |> cmdNone

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
                            Tuple.first buf.view.cursor + 1

                         else
                            Tuple.first buf.view.cursor
                        )
                    )
                |> cmdNone

        JumpByView factor ->
            ( { ed | buf = jumpByView factor global buf }, Cmd.none )

        Put forward ->
            ( ed
                |> put register forward
            , Cmd.none
            )

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
                                        | global =
                                            { global
                                                | exHistory =
                                                    case global.exHistory of
                                                        h :: rest ->
                                                            if s == h then
                                                                global.exHistory

                                                            else
                                                                (s :: global.exHistory)
                                                                    |> List.take 50

                                                        _ ->
                                                            [ s ]
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
                |> updateGlobal (updateIme (setImeActive False))
            , Cmd.none
            )

        ToggleTip ->
            ( updateGlobal (Buf.setShowTip (not global.showTip)) ed
            , Cmd.none
            )

        SelectAutoComplete forward ->
            ( updateBuffer (handleSelectWord forward) ed
            , Cmd.none
            )

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
                    buf.view.cursor
            in
            ( updateBuffer
                (Buf.infoMessage
                    ((buf |> Buf.shortBufferPath global |> quote)
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
                        (\ime -> setImeActive (not <| isImeActive ime) ime)
                        global
              }
            , Cmd.none
            )

        SelectHistory forward ->
            ( updateBuffer (handleSelectHistory forward global.exHistory) ed
            , Cmd.none
            )

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
                        Buf.updateView (View.setCursor ( minY, minX ) True) buf

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
                            |> Buf.updateView (View.setCursor ( minY, x ) True)

                _ ->
                    buf

        _ ->
            buf


isExMode mode =
    case mode of
        Ex _ ->
            True

        _ ->
            False


applyEdit : Maybe Int -> Maybe Operator -> String -> Editor -> ( Editor, Cmd Msg )
applyEdit count edit register ({ buf } as ed) =
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
editTestBuffer path ({ buf, global } as ed) =
    let
        ( global1, buf1 ) =
            createBuffer path global

        global2 =
            Buf.addBuffer buf1 global1
    in
    jumpToPath
        True
        (path
            |> normalizePath global.pathSeperator
            |> replaceHomeDir global.homedir
        )
        Nothing
        { ed | global = global2 }


splitFirstSpace : String -> ( String, String )
splitFirstSpace str =
    str
        |> String.trim
        |> P.run
            (P.succeed
                (\a b c s ->
                    ( String.slice 0 a s
                    , String.slice b c s
                    )
                )
                |. P.chompWhile notSpace
                |= P.getOffset
                |. P.chompWhile isSpace
                |= P.getOffset
                |. P.chompWhile notSpace
                |= P.getOffset
                |= P.getSource
            )
        |> Result.withDefault ( str, "" )


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
    case splitFirstSpace str of
        ( "e", path ) ->
            edit path

        ( "o", path ) ->
            edit path

        ( "b", path ) ->
            edit path

        ( "w", "" ) ->
            ( ed, sendWriteBuffer global.service buf.path buf )

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
            ( ed, sendSearch global.service global.cwd s )

        ( "copy", "" ) ->
            yankWholeBuffer ed

        ( "cd", "" ) ->
            ( updateBuffer (Buf.infoMessage global.cwd) ed
            , Cmd.none
            )

        ( "cd", cwd ) ->
            ( ed
            , cwd
                |> replaceHomeDir global.homedir
                |> resolvePath
                    global.pathSeperator
                    global.cwd
                |> sendCd global.service
            )

        ( "mkdir", "" ) ->
            ( { ed | buf = Buf.errorMessage "Argument Error: mkdir path" buf }
            , Cmd.none
            )

        ( "mkdir", path ) ->
            ( ed
            , path
                |> replaceHomeDir global.homedir
                |> resolvePath
                    global.pathSeperator
                    global.cwd
                |> sendMkDir global.service
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


cleanBuffers : Global -> Global
cleanBuffers global =
    let
        bufIds =
            global.window
                |> Win.toList
                |> List.concatMap (\item -> List.map .bufId item.frame.views)
                |> Set.fromList
    in
    { global
        | buffers =
            Dict.filter
                (\id b ->
                    (b
                        |> getBufferId
                        |> isTempBuffer
                    )
                        || Set.member id bufIds
                )
                global.buffers
    }


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



--log : String -> (a -> b) -> a -> a
--log message selector a =
--    let
--        _ =
--            Debug.log message (selector a)
--    in
--    a


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


updateDebouncers debouncers ({ global } as ed) =
    { ed | global = { global | debouncers = debouncers } }


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
                |> Maybe.map isLineDeltaMotion
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
                        Buf.updateView
                            (updateViewAfterCursorChanged
                                ed.global.lineHeight
                                buf1.mode
                                buf1.lines
                                buf1.syntax
                            )
                            buf1
                    )
                >> updateGlobal (saveDotRegister replaying)
                >> applyDiff
                >> updateScrollLeftPx
            )
        |> Tuple.mapSecond List.singleton
        |> doLint buf
        |> doTokenize buf
        |> doFocusIme ed
        |> Tuple.mapSecond Cmd.batch


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

            history =
                buf.history

            view =
                buf.view
        in
        { ed
            | buf =
                { buf
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
                            |> Maybe.map (min buf.syntaxDirtyFrom)
                            |> Maybe.withDefault buf.syntaxDirtyFrom
                    , view =
                        { view
                            | lines =
                                Buf.applyDiffToView
                                    diff
                                    view.scrollTop
                                    view.size.height
                                    view.lines
                        }
                }
            , global =
                { global1
                    | lint =
                        { items =
                            Buf.applyPatchesToLintErrors
                                global1.lint.items
                                diff
                        , count = global1.lint.count
                        }
                    , locationList =
                        applyPatchesToLocations
                            buf.path
                            diff
                            global1.locationList
                }
        }


onTokenized : Editor -> Result error TokenizeResponse -> ( Editor, Cmd Msg )
onTokenized ({ buf, global } as ed) resp =
    case resp of
        Ok payload ->
            case payload of
                TokenizeSuccess n syntax ->
                    ( let
                        syntax1 =
                            buf.syntax
                                |> Array.slice 0 n
                                |> (\s -> Array.append s syntax)
                      in
                      { ed
                        | buf =
                            { buf
                                | syntax = syntax1
                                , syntaxDirtyFrom = Array.length syntax1
                                , view =
                                    pairCursor
                                        buf.mode
                                        buf.lines
                                        buf.syntax
                                        buf.view
                            }
                      }
                    , Cmd.none
                    )

                TokenizeLineSuccess begin tokens ->
                    let
                        ( debouncers, cmd ) =
                            debounceTokenize
                                Debouncing
                                ed.global.debouncers
                                100
                    in
                    ( { ed
                        | buf =
                            { buf
                                | syntax =
                                    Array.set begin tokens buf.syntax
                                , syntaxDirtyFrom = begin + 1
                            }
                        , global = { global | debouncers = debouncers }
                      }
                    , cmd
                    )

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


tokenizeBuffer : Editor -> ( Editor, Cmd Msg )
tokenizeBuffer ed =
    ( ed, tokenizeBufferCmd ed.buf.syntaxDirtyFrom ed.global.service ed.buf )


applyLintItems : List LintError -> Buffer -> Global -> Global
applyLintItems items buf global =
    let
        normalizeFile file =
            if
                String.isEmpty file
                    || String.endsWith buf.path file
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
                            (\x -> x - 1)
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


onMouseWheel : Win.Path -> Int -> Int -> Editor -> ( Editor, Cmd Msg )
onMouseWheel path deltaY deltaX ({ buf, global } as ed) =
    let
        view =
            buf.view

        lineHeight =
            global.lineHeight

        scrollTopPx =
            (view.scrollTopPx + deltaY)
                |> max 0
                |> min ((B.count buf.lines - 2) * lineHeight)

        scrollTopDelta =
            scrollTopPx // lineHeight - buf.view.scrollTop
    in
    { ed
        | buf =
            { buf
                | view =
                    { view
                        | scrollTopPx = scrollTopPx

                        --, scrollLeftPx =
                        --    (view.scrollLeftPx + deltaX)
                        --        |> max 0
                    }
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


updateGlobalAfterChange : Win.Path -> Buffer -> Global -> Buffer -> Global -> Global
updateGlobalAfterChange path oldBuf oldGlobal buf global =
    let
        window =
            Win.updateFrame path
                (Frame.updateView buf.id <| always buf.view)
                global.window
    in
    { global
        | buffers =
            if Dict.member buf.id global.buffers then
                Dict.insert buf.id (Loaded buf) global.buffers

            else
                global.buffers
        , window = window
    }


withEditor : (Editor -> ( Editor, Cmd Msg )) -> Global -> ( Global, Cmd Msg )
withEditor fn global =
    withEditorByView global.window.path fn global


withEditorByView :
    Win.Path
    -> (Editor -> ( Editor, Cmd Msg ))
    -> Global
    -> ( Global, Cmd Msg )
withEditorByView path fn global =
    global.window
        |> Win.getFrame path
        |> Maybe.andThen
            (\frame ->
                frame
                    |> Frame.getActiveView
                    |> Maybe.map (\view -> { view | size = frame.size })
            )
        |> Maybe.andThen
            (\view ->
                global.buffers
                    |> getBuffer view.bufId
                    |> Maybe.map (\buf -> { buf | view = view })
            )
        |> Maybe.map
            (\buf ->
                { global = global
                , buf = buf
                }
                    |> fn
                    |> Tuple.mapFirst
                        (\ed ->
                            updateGlobalAfterChange path
                                buf
                                global
                                ed.buf
                                ed.global
                        )
            )
        |> Maybe.withDefault ( global, Cmd.none )


updateActiveBuffer : (Buffer -> Buffer) -> Global -> Global
updateActiveBuffer fn global =
    getActiveBuffer global
        |> Maybe.map
            (\buf -> updateGlobalAfterChange global.window.path buf global (fn buf) global)
        |> Maybe.withDefault global


update : Msg -> Global -> ( Global, Cmd Msg )
update message global =
    case message of
        Debouncing debounceMessage payload ->
            debouncerUpdate
                { toMsg = Debouncing
                , toModel =
                    \debounceres -> { global | debouncers = debounceres }
                , toMsgFromDebounced = Debounce
                }
                update
                debounceMessage
                payload
                global.debouncers

        MouseWheel path deltaY deltaX ->
            withEditorByView path (onMouseWheel path deltaY deltaX) global

        PressKeys keys ->
            withEditor
                (\ed ->
                    List.foldl
                        (\key_ ( ed_, cmds ) ->
                            let
                                ( ed__, cmd ) =
                                    handleKeypress False key_ ed_
                            in
                            ( ed__, cmd :: cmds )
                        )
                        ( ed, [] )
                        (mapKeys ed.buf.mode keys)
                        |> Tuple.mapSecond (List.reverse >> Cmd.batch)
                )
                global

        Resize size ->
            --todo tokenizeBuffer
            ( onResize size global, Cmd.none )

        ReadClipboard result ->
            case result of
                Ok { replaying, key, ast, s } ->
                    withEditor
                        (\ed ->
                            { ed | global = Buf.setRegister "+" (Text s) global }
                                |> applyVimAST replaying key ast
                        )
                        global

                Err s ->
                    ( global, Cmd.none )

        WriteClipboard _ ->
            ( global, Cmd.none )

        Read result ->
            ( onRead result global, Cmd.map IMEMessage focusIme )

        Write result ->
            withEditor (onWrite result) global

        Lint id resp ->
            withEditor
                (\ed ->
                    ( updateGlobal (onLint id resp ed.buf) ed
                    , Cmd.none
                    )
                )
                global

        Tokenized ( bufId, version ) resp ->
            withEditor
                (\({ buf } as ed) ->
                    if
                        (bufId == buf.id)
                            && (version == buf.history.version)
                    then
                        onTokenized ed resp

                    else
                        ( ed, Cmd.none )
                )
                global

        Debounce SendTokenize ->
            withEditor
                (\({ buf } as ed) ->
                    ( ed
                    , tokenizeBufferCmd buf.syntaxDirtyFrom global.service buf
                    )
                )
                global

        Debounce SendLint ->
            withEditor
                (\({ buf } as ed) ->
                    if buf.config.lint then
                        ( ed
                        , sendLintOnTheFly
                            global.service
                            global.pathSeperator
                            buf.id
                            buf.path
                            buf.history.version
                            buf.lines
                        )

                    else
                        ( ed, Cmd.none )
                )
                global

        Debounce PersistentAll ->
            ( persistentAll global, Cmd.none )

        ReadTags result ->
            withEditor (onReadTags result) global

        SearchResult result ->
            withEditor (onSearch result) global

        ListAllFiles resp ->
            case resp of
                Ok files ->
                    ( updateActiveBuffer
                        (startExAutoComplete 2 global.cwd files)
                        global
                    , Cmd.none
                    )

                Err _ ->
                    ( global, Cmd.none )

        ListDirectries resp ->
            case resp of
                Ok files ->
                    ( updateActiveBuffer
                        (startAutoCompleteFiles files global)
                        global
                    , Cmd.none
                    )

                Err _ ->
                    ( global, Cmd.none )

        ListFiles resp ->
            case resp of
                Ok files ->
                    ( updateActiveBuffer
                        (startAutoCompleteFiles files global)
                        global
                    , Cmd.none
                    )

                Err _ ->
                    ( global, Cmd.none )

        ListBuffers files ->
            ( updateActiveBuffer (startExAutoComplete 2 global.cwd files) global
            , Cmd.none
            )

        SetCwd (Ok cwd) ->
            ( { global | cwd = cwd }
                |> updateActiveBuffer (Buf.infoMessage cwd)
            , Cmd.none
            )

        IMEMessage imeMsg ->
            Ime.update IMEMessage (PressKeys >> toCmd) imeMsg global.ime
                |> Tuple.mapFirst (\ime -> { global | ime = ime })

        FocusIme ->
            ( global, Cmd.map IMEMessage focusIme )

        SetCwd _ ->
            ( global, Cmd.none )

        MakeDir res ->
            case res of
                Ok _ ->
                    ( global, Cmd.none )

                Err err ->
                    ( updateActiveBuffer (Buf.errorMessage err) global
                    , Cmd.none
                    )

        NoneMessage ->
            ( global, Cmd.none )

        BootMessage _ ->
            ( global, Cmd.none )


onResize : Size -> Global -> Global
onResize size global =
    let
        size1 =
            { size
                | height =
                    size.height
                        - (global.statusbarHeight * global.lineHeight)
            }
    in
    { global
        | window =
            resizeViews
                size1
                global.lineHeight
                global.window
        , size = size1
    }


onLint : BufferIdentifier -> Result a (List LintError) -> Buffer -> Global -> Global
onLint ( bufId, version ) resp buf global =
    if
        (bufId == buf.id)
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
            let
                path =
                    "[Search]"

                edit =
                    Buf.transaction
                        [ Insertion ( 0, 0 ) <|
                            B.fromString <|
                                s
                                    ++ String.repeat 80 "-"
                                    ++ "\n"
                                    ++ "\n"
                        ]

                window =
                    ed.global.window
                        |> Win.getActiveFrame
                        |> Maybe.map
                            (\frame ->
                                ed.global.window
                                    |> Win.hsplit 0.3 frame
                                    |> Win.activeNextFrame
                            )
                        |> Maybe.withDefault ed.global.window

                global_ =
                    ed.global
            in
            { ed
                | global = { global_ | window = window }
            }
                |> jumpToPath False
                    path
                    Nothing
                |> Tuple.mapFirst
                    (\({ global } as ed1) ->
                        { ed1
                            | global =
                                { global
                                    | buffers =
                                        Dict.update
                                            path
                                            (Maybe.andThen
                                                (getLoadedBuffer
                                                    >> Maybe.map (edit >> Loaded)
                                                )
                                            )
                                            global.buffers
                                    , window =
                                        resizeViews global.size
                                            global.lineHeight
                                            global.window
                                }
                        }
                    )

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
                                                , cursor = buf_.view.cursor
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


resizeViews : Size -> Int -> Win.Window Frame -> Win.Window Frame
resizeViews size lineHeight =
    Win.mapFrame
        (\frame ( widthPercent, heightPercent ) ->
            let
                frameSize =
                    { width = ceiling <| toFloat size.width * widthPercent
                    , height =
                        (ceiling <| toFloat size.height * heightPercent)
                            // lineHeight
                    }
            in
            Frame.updateActiveView
                (resizeView frameSize
                    >> scrollToCursor lineHeight
                )
                { frame | size = frameSize }
        )



--logGlobal : String -> Global -> Global
--logGlobal message global =
--    let
--        _ =
--            Debug.log (message ++ " buffers keys") (Dict.keys global.buffers)
--
--        _ =
--            Debug.log (message ++ " buffers")
--                (Dict.values global.buffers
--                    |> List.map
--                        (\b ->
--                            case b of
--                                Loaded b1 ->
--                                    "[loaded]" ++ b1.path
--
--                                NotLoad b1 ->
--                                    "[not load]" ++ b1.path
--                        )
--                )
--
--    in
--    global
--logEd2 : String -> ( Editor, Cmd a ) -> ( Editor, Cmd a )
--logEd2 message (( ed, _ ) as res) =
--    let
--        _ =
--            logEd message ed
--    in
--    res
--
--
--logEd : String -> Editor -> Editor
--logEd message ed =
--    let
--        _ =
--            Debug.log (message ++ " buffers") (Dict.keys ed.global.buffers)
--
--        _ =
--            Debug.log (message ++ " buffers")
--                (Dict.values ed.global.buffers
--                    |> List.map
--                        (\b ->
--                            case b of
--                                Loaded b1 ->
--                                    "[loaded]" ++ b1.path
--
--                                NotLoad b1 ->
--                                    "[not load]" ++ b1.path
--                        )
--                )
--
--        _ =
--            Debug.log (message ++ " buf.view.scrollTop") ed.buf.view.scrollTop
--    in
--    ed


isExculdLineBreak : Mode -> Bool
isExculdLineBreak mode =
    case mode of
        Visual _ ->
            False

        Insert _ ->
            False

        _ ->
            True


updateViewAfterCursorChanged : Int -> Mode -> B.TextBuffer -> Syntax -> View -> View
updateViewAfterCursorChanged lineHeight mode lines syntax =
    correctCursor (isExculdLineBreak mode) lines
        >> scrollToCursor lineHeight
        >> pairCursor mode lines syntax


restoreBufferHistory : Int -> Buffer -> Buffer
restoreBufferHistory lineHeight buf =
    buf
        |> Buf.transaction buf.history.changes
        |> Buf.updateHistory (always buf.history)
        |> (\buf1 ->
                Buf.updateView
                    (View.setCursor buf.view.cursor True
                        >> updateViewAfterCursorChanged
                            lineHeight
                            buf1.mode
                            buf1.lines
                            buf1.syntax
                    )
                    buf1
           )


onRead : Result Http.Error ( Win.Path, Buffer ) -> Global -> Global
onRead result global =
    case result of
        Ok ( framePath, buf ) ->
            let
                --_ =
                --Debug.log "onRead" ( setActive, buf.path )
                buf2 =
                    restoreBufferHistory global.lineHeight buf

                global2 =
                    Buf.addBuffer buf2 global
            in
            { global2
                | window =
                    Win.updateFrame
                        framePath
                        (\frame ->
                            Frame.addOrActiveView
                                buf2.view
                                frame
                        )
                        global2.window
            }

        Err err ->
            updateActiveBuffer (Buf.errorMessage <| httpErrorMessage err) global


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
                        |> (\buf2 ->
                                Buf.updateView
                                    (View.setCursor buf.view.cursor True
                                        >> updateViewAfterCursorChanged
                                            global.lineHeight
                                            buf2.mode
                                            buf2.lines
                                            buf2.syntax
                                    )
                                    buf2
                           )
                        |> Buf.infoMessage
                            ((buf |> Buf.shortBufferPath global |> quote) ++ " Written")

                lintCmd =
                    if buf1.config.lint then
                        sendLintProject global.service
                            global.pathSeperator
                            buf1.id
                            buf1.path
                            buf1.history.version
                            buf1.lines

                    else
                        Cmd.none

                ed1 =
                    { ed | buf = buf1 }
                        |> applyDiff
            in
            ( ed1
            , Cmd.batch
                [ lintCmd
                , tokenizeBufferCmd
                    ed1.buf.syntaxDirtyFrom
                    ed1.global.service
                    ed1.buf
                ]
            )

        _ ->
            ( updateBuffer
                (Buf.errorMessage
                    (Buf.shortBufferPath global buf ++ " save error")
                )
                ed
            , Cmd.none
            )


init : Flags -> String -> FontInfo -> Size -> ServerArgs -> ( Global, Cmd Msg )
init flags theme fontInfo size args =
    let
        { homedir, pathSeperator } =
            args

        { cwd, service, buffers } =
            flags

        { window, registers, exHistory } =
            flags

        --|> Debug.log "flags"
        lineHeight =
            fontInfo.lineHeight

        viewHeight =
            size.height // lineHeight - emptyGlobal.statusbarHeight

        decodedBuffers =
            buffers
                |> Decode.decodeValue
                    (bufferDecoder
                        pathSeperator
                        homedir
                        |> Decode.list
                    )
                |> Result.map
                    (List.map <|
                        \b ->
                            if isTempBuffer b.path then
                                ( b.id
                                , { b | config = Buf.disableSyntax b.config }
                                    |> restoreBufferHistory lineHeight
                                    |> Loaded
                                )

                            else
                                ( b.id, NotLoad b )
                    )
                |> Result.withDefault [ ( "", Loaded emptyBuffer ) ]

        decodedWindow =
            window
                |> Decode.decodeValue (windowDecoder lineHeight)
                |> Result.withDefault
                    (Win.initWindow
                        { emptyFrame | views = [ emptyView ] }
                    )
                |> resizeViews size lineHeight

        dictBuffers =
            Dict.fromList decodedBuffers
    in
    ( { emptyGlobal
        | service = service
        , exHistory = exHistory
        , cwd =
            if String.isEmpty cwd then
                homedir

            else
                cwd
        , pathSeperator = pathSeperator
        , fontInfo = fontInfo
        , homedir = homedir
        , registers =
            Decode.decodeValue registersDecoder registers
                |> Result.withDefault Dict.empty
        , lineHeight = lineHeight
        , window = decodedWindow
        , ime = emptyIme
        , buffers = dictBuffers
        , theme = theme
      }
        |> onResize size
    , decodedWindow
        |> Win.toList
        |> List.filterMap
            (\w ->
                w.frame
                    |> Frame.getActiveView
                    |> Maybe.andThen
                        (\({ bufId } as view) ->
                            dictBuffers
                                |> Dict.get bufId
                                |> Maybe.andThen getNotLoadBuffer
                                |> Maybe.map
                                    (\buf ->
                                        sendReadBuffer service
                                            viewHeight
                                            w.path
                                            { buf | view = view }
                                    )
                        )
            )
        |> ((::) <| Cmd.map IMEMessage focusIme)
        |> Cmd.batch
    )


gutterWidth : B.TextBuffer -> Int
gutterWidth lines =
    let
        totalLines =
            B.count lines - 1
    in
    totalLines |> String.fromInt |> String.length


updateScrollLeftPx : Editor -> Editor
updateScrollLeftPx ({ global, buf } as ed) =
    { ed
        | buf =
            Buf.updateView
                (\view ->
                    { view | scrollLeftPx = getScrollLeftPx global.fontInfo buf }
                )
                buf
    }


getScrollLeftPx : FontInfo -> Buffer -> Int
getScrollLeftPx fontInfo buf =
    let
        view =
            buf.view

        maybeCursor =
            case buf.mode of
                Ex _ ->
                    Nothing

                _ ->
                    Just view.cursor

        relativeGutterWidth =
            4
    in
    maybeCursor
        |> Maybe.map
            (\( y, x ) ->
                let
                    ( ( _, leftPx ), ( _, rightPx ) ) =
                        cursorPoint fontInfo buf.lines y x

                    widthPx =
                        toFloat view.size.width
                            - toFloat
                                (gutterWidth buf.lines
                                    + relativeGutterWidth
                                    + 1
                                )
                            * charWidth fontInfo '0'
                            |> floor
                in
                if leftPx < view.scrollLeftPx then
                    leftPx

                else if rightPx > widthPx + view.scrollLeftPx then
                    rightPx - widthPx

                else
                    view.scrollLeftPx
            )
        |> Maybe.withDefault 0
