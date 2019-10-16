module Update exposing (init, update, updateActiveBuffer)

import Clipboard
import Debouncers exposing (..)
import Dict exposing (Dict)
import Font exposing (FontInfo)
import Fs
import Helper.Helper exposing (..)
import Http
import Ime exposing (..)
import Internal.Jumps exposing (..)
import Internal.Syntax exposing (Syntax, fromTreeSitter)
import Internal.TextBuffer as B exposing (Patch(..))
import Internal.Window as Win exposing (Window)
import Json.Decode as Decode
import Model exposing (..)
import Model.Buffer exposing (..)
import Model.Frame as Frame exposing (Frame)
import Model.Global exposing (..)
import Model.Lint exposing (..)
import Model.LoadBuffer exposing (..)
import Model.Size exposing (Size)
import Model.View as View exposing (..)
import TreeSitter as TS
import Update.AutoComplete exposing (..)
import Update.Buffer as Buf
import Update.CTag exposing (..)
import Update.Cursor exposing (..)
import Update.Delete exposing (..)
import Update.Insert exposing (..)
import Update.Jump exposing (..)
import Update.Keymap exposing (mapKeys)
import Update.Lint exposing (..)
import Update.Message exposing (..)
import Update.Motion exposing (..)
import Update.Mouse exposing (..)
import Update.Service exposing (..)
import Update.Tokenize exposing (..)
import Update.Vim exposing (..)


updateGlobalAfterChange : Win.Path -> Buffer -> Global -> Buffer -> Global -> Global
updateGlobalAfterChange path oldBuf oldGlobal buf global =
    let
        view1 =
            if oldBuf.view.scrollTop == buf.view.scrollTop then
                buf.view

            else
                updateSyntax buf buf.view

        window =
            Win.updateFrame path
                (Frame.updateView buf.id <| always view1)
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
        |> Maybe.andThen Frame.getActiveView
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
                , toModel = \debounceres -> { global | debouncers = debounceres }
                , toMsgFromDebounced = Debounce
                }
                update
                debounceMessage
                payload
                global.debouncers

        MouseWheel path deltaY deltaX ->
            withEditorByView path (onMouseWheel path deltaY deltaX) global

        MouseClick path ->
            ( onMouseClick path global, Cmd.none )

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

                Err _ ->
                    ( global, Cmd.none )

        WriteClipboard _ ->
            ( global, Cmd.none )

        Read result ->
            ( onRead result global, Cmd.map IMEMessage focusIme )

        Write result ->
            withEditor (onWrite result) global

        Lint id resp ->
            withEditor
                (\ed -> ( { ed | global = onLint id resp ed.buf ed.global }, Cmd.none ))
                global

        Tokenized ( bufId, version ) resp ->
            withEditor
                (\({ buf } as ed) ->
                    if (bufId == buf.id) && (version == buf.history.version) then
                        onTokenized ed resp

                    else
                        ( ed, Cmd.none )
                )
                global

        Debounce SendTokenize ->
            withEditor
                (\({ buf } as ed) ->
                    ( ed, tokenizeBufferCmd buf.syntaxDirtyFrom global.service buf )
                )
                global

        Debounce SendLint ->
            withEditor
                (\({ buf } as ed) ->
                    if buf.config.lint then
                        ( ed
                        , sendLintOnTheFly
                            global.service
                            (Fs.pathSeperator global.fs)
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
                        (startExAutoComplete 2 (Fs.workingDir global.fs) files)
                        global
                    , Cmd.none
                    )

                Err _ ->
                    ( global, Cmd.none )

        ListDirs resp ->
            case resp of
                Ok files ->
                    ( updateActiveBuffer (startAutoCompleteFiles files global) global
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
            ( updateActiveBuffer (startExAutoComplete 2 (Fs.workingDir global.fs) files) global
            , Cmd.none
            )

        SetCwd (Ok cwd) ->
            ( { global | fs = Fs.setWorkingDir cwd global.fs }
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
                    ( updateActiveBuffer (Buf.errorMessage err) global, Cmd.none )

        NoneMessage ->
            ( global, Cmd.none )

        BootMessage _ ->
            ( global, Cmd.none )


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
            { ed | global = { global_ | window = window } }
                |> jumpToPath False path Nothing
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
                                        global.window
                                            |> resizeViews global.size global.lineHeight
                                            |> mapFrontViews global.buffers updateSyntax
                                }
                        }
                    )

        _ ->
            ( ed, Cmd.none )


updateViewAfterCursorChanged : Mode -> B.TextBuffer -> Syntax -> View -> View
updateViewAfterCursorChanged mode lines syntax =
    correctCursor (isExculdLineBreak mode) lines
        >> scrollToCursor
        >> pairCursor mode lines syntax


restoreBufferHistory : Buffer -> Buffer
restoreBufferHistory buf =
    buf
        |> Buf.transaction buf.history.changes
        |> Buf.updateHistory (always buf.history)
        |> (\buf1 ->
                { buf1
                    | view =
                        buf1.view
                            |> View.setCursor buf.view.cursor True
                            |> updateViewAfterCursorChanged
                                buf1.mode
                                buf1.lines
                                buf1.syntax
                }
           )


onRead : Result String ( Win.Path, Buffer ) -> Global -> Global
onRead result global =
    case result of
        Ok ( framePath, buf ) ->
            let
                buf1 =
                    restoreBufferHistory buf

                parser =
                    buf1.name
                        |> extname
                        |> String.dropLeft 1
                        |> TS.createParser
                        |> Maybe.withDefault TS.dummyParser

                s =
                    B.toString buf1.lines

                buf2 =
                    { buf1
                        | treeSitter =
                            { parser = parser
                            , tree =
                                TS.parse parser
                                    (\t arg ->
                                        case arg.endIndex of
                                            Just endIndex ->
                                                ( String.slice arg.startIndex endIndex s, t )

                                            _ ->
                                                ( String.dropLeft arg.startIndex s, t )
                                    )
                                    buf1.treeSitter.tree
                            }
                    }

                global2 =
                    Buf.addBuffer buf2 global
            in
            { global2
                | window =
                    Win.updateFrame
                        framePath
                        (Frame.addOrActiveView <| updateSyntax buf2 buf2.view)
                        global2.window
            }

        Err err ->
            updateActiveBuffer (Buf.errorMessage err) global


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
                            (Fs.pathSeperator global.fs)
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

                --, tokenizeBufferCmd
                --    ed1.buf.syntaxDirtyFrom
                --    ed1.global.service
                --    ed1.buf
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


mapFrontViews : Dict String LoadBuffer -> (Buffer -> View -> View) -> Window Frame -> Window Frame
mapFrontViews buffers fn window =
    Win.mapFrame
        (\frame _ ->
            Frame.updateActiveView
                (\view ->
                    Dict.get view.bufId buffers
                        |> Maybe.map
                            (\buf ->
                                case buf of
                                    Loaded b ->
                                        fn b view

                                    _ ->
                                        view
                            )
                        |> Maybe.withDefault view
                )
                frame
        )
        window


updateSyntax : Buffer -> View -> View
updateSyntax buf view =
    { view
        | syntax =
            fromTreeSitter
                view.scrollTop
                (view.scrollTop + view.size.height)
                buf.treeSitter.tree
    }


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
                    (Decode.list <| bufferDecoder fs)
                |> Result.map
                    (List.map <|
                        \b ->
                            if isTempBuffer b.path then
                                ( b.id
                                , b
                                    |> restoreBufferHistory
                                    |> Loaded
                                )

                            else
                                let
                                    parser =
                                        b.path
                                            |> extname
                                            |> String.dropLeft 1
                                            |> TS.createParser
                                            |> Maybe.withDefault TS.dummyParser
                                in
                                ( b.id
                                , NotLoad
                                    { b
                                        | treeSitter =
                                            { parser = parser
                                            , tree = TS.parse parser (\t _ -> ( "", t )) b.treeSitter.tree
                                            }
                                    }
                                )
                    )
                |> Result.withDefault [ ( "", Loaded emptyBuffer ) ]

        decodedWindow =
            window
                |> Decode.decodeValue windowDecoder
                |> Result.withDefault
                    (Win.initWindow <| Frame.addOrActiveView emptyView Frame.empty)
                |> resizeViews size lineHeight

        dictBuffers =
            Dict.fromList decodedBuffers

        fs =
            Fs.fileSystem service pathSeperator homedir cwd
    in
    ( { emptyGlobal
        | service = service
        , fs = fs
        , exHistory = exHistory
        , fontInfo = fontInfo
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
                                            fs
                                            viewHeight
                                            w.path
                                            { buf | view = view }
                                    )
                        )
            )
        |> ((::) <| Cmd.map IMEMessage focusIme)
        |> Cmd.batch
    )
