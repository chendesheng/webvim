module Update.Jump exposing (..)

import Model exposing (..)
import Update.Message exposing (..)
import Helper.Helper
    exposing
        ( filename
        , resolvePath
        , floorFromZero
        , keepOneOrMore
        , extname
        , relativePath
        , isPathChar
        )
import Internal.TextBuffer as B exposing (Patch(..))
import Update.Buffer as Buf
import Dict exposing (Dict)
import Parser as P exposing ((|.), (|=), Parser)
import Update.Service exposing (..)
import Array as Array exposing (Array)
import Internal.Jumps
    exposing
        ( saveJump
        , jumpForward
        , jumpBackward
        , Jumps
        , Location
        , currentLocation
        )
import Internal.Position exposing (Position)
import Update.Cursor exposing (correctCursor, scrollToCursor)
import Update.Motion
    exposing
        ( setVisualEnd
        , wordStringUnderCursor
        , wORDStringUnderCursor
        )


isTempBuffer : String -> Bool
isTempBuffer path =
    String.isEmpty path || path == "[Search]"


tokenizeBufferCmd : Editor -> Cmd Msg
tokenizeBufferCmd ({ buf, global } as ed) =
    if isTempBuffer buf.path || not buf.config.syntax then
        Cmd.none
    else
        let
            begin =
                buf.syntaxDirtyFrom

            end =
                Buf.finalScrollTop global.size buf + 2 * global.size.height

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
                    global.service
                    { path = buf.path
                    , version = buf.history.version
                    , line = begin
                    , lines =
                        buf.lines
                            |> B.sliceLines begin end
                            |> B.toString
                    }


tokenizeBuffer : Editor -> ( Editor, Cmd Msg )
tokenizeBuffer ed =
    ( ed, tokenizeBufferCmd ed )


jumpTo : Bool -> BufferInfo -> Editor -> ( Editor, Cmd Msg )
jumpTo isSaveJump info ({ global, buf } as ed) =
    let
        { path, cursor } =
            info

        jumps =
            if isSaveJump then
                saveJump { path = buf.path, cursor = buf.cursor } global.jumps
            else
                global.jumps
    in
        if path == buf.path then
            { ed
                | global = { global | jumps = jumps }
                , buf =
                    buf
                        |> Buf.setCursor cursor True
                        |> Buf.setScrollTop
                            (Buf.bestScrollTop (Tuple.first cursor)
                                global.size.height
                                buf.lines
                                buf.view.scrollTop
                            )
                            global
            }
                |> tokenizeBuffer
        else
            editBuffer False
                info
                { ed | global = { global | jumps = jumps } }


jumpToLocation : Bool -> Location -> Editor -> ( Editor, Cmd Msg )
jumpToLocation isSaveJump { path, cursor } ed =
    jumpToPath isSaveJump path (Just cursor) ed


jumpToPath : Bool -> String -> Maybe Position -> Editor -> ( Editor, Cmd Msg )
jumpToPath isSaveJump path_ overrideCursor ({ global } as ed) =
    let
        path =
            if isTempBuffer path_ then
                path_
            else
                resolvePath
                    global.pathSeperator
                    global.cwd
                    path_

        info =
            global.buffers
                |> Dict.get path
                |> Maybe.map
                    (\info_ ->
                        { info_
                            | cursor =
                                Maybe.withDefault info_.cursor overrideCursor
                        }
                    )
                |> Maybe.withDefault
                    { emptyBufferInfo
                        | path = path
                        , cursor = Maybe.withDefault ( 0, 0 ) overrideCursor
                    }
    in
        jumpTo isSaveJump info ed


editBuffer : Bool -> BufferInfo -> Editor -> ( Editor, Cmd Msg )
editBuffer restoreHistory info ({ global, buf } as ed) =
    if info.path /= "" && info.content == Nothing then
        ( ed
        , sendReadBuffer global.service
            (Tuple.first info.cursor + global.size.height * 2)
            buf.config.tabSize
            info
        )
    else
        let
            global1 =
                { global
                    | buffers =
                        global.buffers
                            |> Dict.remove info.path
                            |> Dict.insert buf.path
                                { path = buf.path
                                , content =
                                    Just
                                        ( buf.lines, buf.syntax )
                                , cursor = buf.cursor
                                , syntax = buf.config.syntax
                                , history = buf.history
                                }
                    , registers =
                        global.registers
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

            newbuf =
                newBuffer info global1

            newbuf1 =
                if restoreHistory then
                    newbuf
                        |> Buf.transaction newbuf.history.changes
                        |> Buf.setCursor newbuf.cursor False
                        |> Buf.updateHistory (always newbuf.history)
                else
                    newbuf
        in
            ( { ed | buf = newbuf1, global = global1 }
            , Cmd.batch
                [ if newbuf.config.lint then
                    sendLintProject global1.service
                        global1.pathSeperator
                        newbuf.path
                        newbuf.history.version
                        newbuf.lines
                  else
                    Cmd.none
                , tokenizeBufferCmd { ed | buf = newbuf }
                ]
            )


isLintEnabled : Global -> String -> Bool -> Bool
isLintEnabled global name lint =
    if lint && extname name == ".elm" then
        name
            |> relativePath global.pathSeperator global.homedir
            |> String.startsWith (".elm" ++ global.pathSeperator)
            |> not
    else
        lint


newBuffer : BufferInfo -> Global -> Buffer
newBuffer info global =
    let
        { cursor, path, content, history } =
            info

        ( name, ext ) =
            filename path

        config =
            Buf.configs
                |> Dict.get ext
                |> Maybe.withDefault defaultBufferConfig

        config1 =
            { config
                | syntax = info.syntax
            }

        ( lines, syntax ) =
            Maybe.withDefault ( emptyBuffer.lines, emptyBuffer.syntax ) content

        height =
            global.size.height

        scrollTop =
            Buf.bestScrollTop (Tuple.first cursor)
                height
                lines
                0
    in
        { lines = lines
        , mode = Normal { message = EmptyMessage }
        , config =
            { config1
                | lint = isLintEnabled global path config1.lint
            }
        , view =
            { emptyView
                | scrollTopPx = scrollTop * global.lineHeight
                , scrollTop = scrollTop
                , lines =
                    List.range scrollTop (scrollTop + height + 1)
            }
        , cursor = cursor
        , cursorColumn = Tuple.second cursor
        , path = path
        , name = name ++ ext
        , history = history
        , syntax = syntax
        , syntaxDirtyFrom = Array.length syntax
        , continuation = ""
        , dirtyIndent = 0
        , motionFailed = False
        }
            |> correctCursor
            |> scrollToCursor global


jumpByView : Float -> Global -> Buffer -> Buffer
jumpByView factor global buf =
    let
        forward =
            factor > 0

        view =
            buf.view

        height =
            global.size.height

        lineScope row =
            row
                |> max 0
                |> min (B.count buf.lines - 1)

        scrollScope scrollTop_ n_ =
            let
                newn =
                    scrollTop_ + n_

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
                    |> Buf.setScrollTop scrollTop global

            Nothing ->
                buf


locationParser : Parser Location
locationParser =
    P.succeed
        (\path ints ->
            { path = path
            , cursor =
                case ints of
                    x :: y :: _ ->
                        ( y - 1, x - 1 )

                    [ y ] ->
                        ( y - 1, 0 )

                    _ ->
                        ( 0, 0 )
            }
        )
        |= keepOneOrMore isPathChar
        |= (P.loop []
                (\locations ->
                    let
                        done =
                            P.succeed <| P.Done locations

                        continue =
                            P.succeed (\loc -> P.Loop <| loc :: locations)
                                |. P.symbol ":"
                                |= P.int
                    in
                        if List.length locations == 2 then
                            done
                        else
                            P.oneOf [ P.backtrackable continue, done ]
                )
           )


jumpHistory : Bool -> Editor -> ( Editor, Cmd Msg )
jumpHistory isForward ({ global, buf } as ed) =
    let
        jumps =
            if isForward then
                jumpForward global.jumps
            else
                jumpBackward
                    { path = buf.path
                    , cursor = buf.cursor
                    }
                    global.jumps
    in
        case currentLocation jumps of
            Just loc ->
                jumpToLocation False
                    loc
                    { ed | global = { global | jumps = jumps } }

            _ ->
                ( ed, Cmd.none )


jumpLastBuffer : Editor -> ( Editor, Cmd Msg )
jumpLastBuffer ({ global } as ed) =
    case Dict.get "#" global.registers of
        Just reg ->
            jumpToPath True (registerString reg) Nothing ed

        _ ->
            ( ed, Cmd.none )


startJumpToTag : Maybe Int -> Editor -> ( Editor, Cmd Msg )
startJumpToTag count ({ buf, global } as ed) =
    case
        wordStringUnderCursor
            buf.config.wordChars
            buf.lines
            buf.cursor
    of
        Just ( _, s ) ->
            ( ed
            , sendReadTags global.service
                global.pathSeperator
                global.cwd
                buf.path
                (Maybe.withDefault 1 count - 1)
                s
            )

        _ ->
            ( ed, Cmd.none )


jumpToFile : Editor -> ( Editor, Cmd Msg )
jumpToFile ({ buf } as ed) =
    case wORDStringUnderCursor buf of
        Just ( _, s ) ->
            case P.run locationParser s of
                Ok loc ->
                    jumpToLocation True loc ed

                _ ->
                    ( ed, Cmd.none )

        _ ->
            ( ed, Cmd.none )
