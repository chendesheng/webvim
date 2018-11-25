module Update.Jump exposing (..)

import Model exposing (..)
import Update.Message exposing (..)
import Helper.Helper
    exposing
        ( fromListBy
        , filename
        , safeRegex
        , isSpace
        , notSpace
        , resolvePath
        , normalizePath
        , nthList
        , floorFromZero
        , keepOneOrMore
        , replaceHomeDir
        , extname
        , relativePath
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
                    buf.global.service
                    { path = buf.path
                    , version = buf.history.version
                    , line = begin
                    , lines =
                        buf.lines
                            |> B.sliceLines begin end
                            |> B.toString
                    }


tokenizeBuffer : Buffer -> ( Buffer, Cmd Msg )
tokenizeBuffer buf =
    ( buf, tokenizeBufferCmd buf )


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
            editBuffer False
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
                    buf.global.pathSeperator
                    buf.global.cwd
                    path_

        info =
            buf.global.buffers
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
        jumpTo isSaveJump info buf


editBuffer : Bool -> BufferInfo -> Buffer -> ( Buffer, Cmd Msg )
editBuffer restoreHistory info buf =
    if info.path /= "" && info.content == Nothing then
        ( buf
        , sendReadBuffer buf.global.service
            (Tuple.first info.cursor + buf.view.size.height * 2)
            buf.config.tabSize
            info
        )
    else
        let
            global =
                buf.global

            newbuf =
                newBuffer
                    info
                    { buf
                        | global =
                            { global
                                | buffers =
                                    (buf.global.buffers
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
                                    )
                                , registers =
                                    buf.global.registers
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
                    }

            newbuf1 =
                if restoreHistory then
                    newbuf
                        |> Buf.transaction newbuf.history.changes
                        |> Buf.setCursor newbuf.cursor False
                        |> Buf.updateHistory (always newbuf.history)
                else
                    newbuf
        in
            ( newbuf1
            , Cmd.batch
                [ if newbuf.config.lint then
                    sendLintProject newbuf.global.service
                        newbuf.global.pathSeperator
                        newbuf.path
                        newbuf.history.version
                        newbuf.lines
                  else
                    Cmd.none
                , tokenizeBufferCmd newbuf
                ]
            )


isLintEnabled gb lint name =
    if lint && extname name == ".elm" then
        name
            |> relativePath gb.pathSeperator gb.homedir
            |> String.startsWith (".elm" ++ gb.pathSeperator)
            |> not
    else
        lint


newBuffer : BufferInfo -> Buffer -> Buffer
newBuffer info buf =
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
            buf.view.size.height

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
                | lint = isLintEnabled buf.global config1.lint path
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
        , history = history
        , syntax = syntax
        , syntaxDirtyFrom = Array.length syntax
        , continuation = ""
        , dirtyIndent = 0
        , locationList = []
        , motionFailed = False
        , jumps = buf.jumps
        , global = buf.global
        }
            |> correctCursor
            |> scrollToCursor


jumpByView : Float -> Buffer -> Buffer
jumpByView factor buf =
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
                    |> Buf.setScrollTop scrollTop

            Nothing ->
                buf


isPathChar : Char -> Bool
isPathChar c =
    notSpace c && (c /= ':')


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


jumpHistory : Bool -> Buffer -> ( Buffer, Cmd Msg )
jumpHistory isForward buf =
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


jumpLastBuffer : Buffer -> ( Buffer, Cmd Msg )
jumpLastBuffer buf =
    case Dict.get "#" buf.global.registers of
        Just reg ->
            jumpToPath True (registerString reg) Nothing buf

        _ ->
            ( buf, Cmd.none )


startJumpToTag : Maybe Int -> Buffer -> ( Buffer, Cmd Msg )
startJumpToTag count buf =
    case
        wordStringUnderCursor
            buf.config.wordChars
            buf.lines
            buf.cursor
    of
        Just ( _, s ) ->
            ( buf
            , sendReadTags buf.global.service
                buf.global.pathSeperator
                buf.global.cwd
                buf.path
                (Maybe.withDefault 1 count - 1)
                s
            )

        _ ->
            ( buf, Cmd.none )


jumpToFile : Buffer -> ( Buffer, Cmd Msg )
jumpToFile buf =
    case wORDStringUnderCursor buf of
        Just ( _, s ) ->
            case P.run locationParser s of
                Ok loc ->
                    jumpToLocation True loc buf

                _ ->
                    ( buf, Cmd.none )

        _ ->
            ( buf, Cmd.none )
