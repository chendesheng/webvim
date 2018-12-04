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
        , findFirst
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


tokenizeBufferCmd : String -> Buffer -> Cmd Msg
tokenizeBufferCmd url buf =
    if isTempBuffer buf.path || not buf.config.syntax then
        Cmd.none
    else
        let
            begin =
                buf.syntaxDirtyFrom

            view =
                buf.view

            end =
                Buf.finalScrollTop view.size buf + 2 * view.size.height

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
                    url
                    { path = buf.path
                    , version = buf.history.version
                    , line = begin
                    , lines =
                        buf.lines
                            |> B.sliceLines begin end
                            |> B.toString
                    }


jumpToLocation : Bool -> Location -> Editor -> ( Editor, Cmd Msg )
jumpToLocation isSaveJump { path, cursor } ed =
    jumpToPath isSaveJump path (Just cursor) ed


jumpToPath : Bool -> String -> Maybe Position -> Editor -> ( Editor, Cmd Msg )
jumpToPath isSaveJump path_ overrideCursor ({ global, buf } as ed) =
    let
        path =
            if isTempBuffer path_ then
                path_
            else
                resolvePath
                    global.pathSeperator
                    global.cwd
                    path_

        updateCursor : Buffer -> Buffer
        updateCursor b =
            { b
                | cursor =
                    Maybe.withDefault b.cursor overrideCursor
            }

        jumps =
            if isSaveJump then
                saveJump { path = buf.path, cursor = buf.cursor } global.jumps
            else
                global.jumps

        global1 =
            { global
                | jumps = jumps
            }
    in
        case
            global1.buffers
                |> Dict.values
                |> findFirst (\b -> b.path == path)
        of
            -- buffer exists
            Just b ->
                ( { ed
                    | buf = updateCursor buf
                    , global =
                        { global1
                            | buffers = Dict.insert b.id b global1.buffers
                            , activeView =
                                Buf.resizeView
                                    global1.activeView.size
                                    b.view
                        }
                  }
                , Cmd.none
                  -- , Cmd.batch
                  --     [ if b.config.lint then
                  --         sendLintProject global.service
                  --             global.pathSeperator
                  --             b.path
                  --             b.history.version
                  --             b.lines
                  --       else
                  --         Cmd.none
                  --     , tokenizeBufferCmd { ed | buf = b }
                  --     ]
                )

            -- buffer not exists
            _ ->
                let
                    ( global2, b ) =
                        createBuffer path global1.activeView.size global1
                in
                    ( { ed | global = { global2 | activeView = b.view } }
                    , b
                        |> updateCursor
                        |> sendReadBuffer
                            global2.service
                            global2.activeView.size.height
                    )
                        |> Debug.log "jumpToPath result"


jumpByView : Float -> Global -> Buffer -> Buffer
jumpByView factor global buf =
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
