module Update.Jump exposing
    ( jumpByView
    , jumpHistory
    , jumpLastBuffer
    , jumpToFile
    , jumpToLocation
    , jumpToPath
    , startJumpToTag
    )

import Array as Array exposing (Array)
import Dict exposing (Dict)
import Helper.Helper
    exposing
        ( extname
        , filename
        , findFirst
        , floorFromZero
        , isPathChar
        , keepOneOrMore
        , relativePath
        , resolvePath
        )
import Internal.Jumps
    exposing
        ( Jumps
        , Location
        , currentLocation
        , jumpBackward
        , jumpForward
        , saveJump
        )
import Internal.Position exposing (Position)
import Internal.TextBuffer as B exposing (Patch(..))
import Internal.Window as Win
import Model exposing (..)
import Parser as P exposing ((|.), (|=), Parser)
import Update.Buffer as Buf
import Update.Message exposing (..)
import Update.Motion
    exposing
        ( setVisualEnd
        , wORDStringUnderCursor
        , wordStringUnderCursor
        )
import Update.Service exposing (..)


{-| multiple cases:

  - buffer exists
      - not loaded
      - loaded
      - same buffer
  - buffer not exists
      - temp buffer (loaded)
      - file buffer (not loaded)

switch to buffer setup:

  - init cursor
  - save jumps
  - resize view
  - save alternative buffer
  - save '#' and '%' registers

setup window:

  - update window
  - insert into buffers

-}
jumpToPath :
    Bool
    -> String
    -> Maybe Position
    -> (View -> Win.Window View -> Win.Window View)
    -> Editor
    -> ( Editor, Cmd Msg )
jumpToPath isSaveJump path_ overrideCursor setView ({ global, buf } as ed) =
    let
        -- always use abolute path
        path =
            if isTempBuffer path_ then
                path_

            else
                resolvePath
                    global.pathSeperator
                    global.cwd
                    path_

        updateCursor : B.TextBuffer -> View -> View
        updateCursor lines view =
            case overrideCursor of
                Just cursor ->
                    jumpToPathSetCursor global.lineHeight cursor lines view

                _ ->
                    view

        global1 =
            { global
                | jumps =
                    if isSaveJump then
                        saveJump
                            { path = buf.path, cursor = buf.view.cursor }
                            global.jumps

                    else
                        global.jumps
            }

        ed1 =
            { ed | global = global1 }
    in
    case jumpToPathFindBuffer global1 path of
        -- buffer exists
        Just ( b, loaded ) ->
            if loaded then
                -- loaded
                if buf.id == b.id then
                    -- same buffer
                    ( { ed1 | buf = Buf.updateView (updateCursor buf.lines) buf }, Cmd.none )

                else
                    jumpToPathBufferLoaded
                        setView
                        updateCursor
                        b
                        buf
                        ed1
                --     [ if b.config.lint then
                --         sendLintProject global.service
                --             global.pathSeperator
                --             b.path
                --             b.history.version
                --             b.lines
                --       else
                --         Cmd.none
                --     ]

            else
                -- not loaded
                jumpToPathBufferNotLoaded updateCursor b buf ed1

        -- buffer not exists
        _ ->
            let
                ( global2, b ) =
                    createBuffer path buf.view.size global1

                ed2 =
                    { ed1 | global = global2 }
            in
            if isTempBuffer path then
                -- loaded
                jumpToPathBufferLoaded setView updateCursor b buf ed2

            else
                -- not loaded
                jumpToPathBufferNotLoaded updateCursor b buf ed2


jumpToPathFindBuffer : Global -> String -> Maybe ( Buffer, Bool )
jumpToPathFindBuffer global path =
    global.buffers
        |> listBuffers
        |> findFirst (\( b, _ ) -> b.path == path)


jumpToPathSetCursor : Int -> Position -> B.TextBuffer -> View -> View
jumpToPathSetCursor lineHeight cursor lines view =
    let
        scrollTop =
            Buf.bestScrollTop
                (Tuple.first cursor)
                view.size.height
                lines
                view.scrollTop
    in
    view
        |> Buf.setCursor cursor True
        |> Buf.setScrollTop scrollTop lineHeight


jumpToPathBufferLoaded :
    (View -> Win.Window View -> Win.Window View)
    -> (B.TextBuffer -> View -> View)
    -> Buffer
    -> Buffer
    -> Editor
    -> ( Editor, Cmd Msg )
jumpToPathBufferLoaded setView updateCursor toBuf buf ed =
    let
        global =
            ed.global

        toBuf1 =
            Buf.updateView
                (\v ->
                    updateCursor toBuf.lines
                        { v | alternativeBuf = Just buf.path }
                )
                toBuf

        registers =
            jumpToPathUpdateRegisters buf.path toBuf.path global.registers
    in
    ( { ed
        | global =
            { global
                | buffers = Dict.insert toBuf1.id (Loaded toBuf1) global.buffers
                , registers = registers
                , window =
                    setView toBuf1.view global.window
            }
      }
    , Cmd.none
    )


jumpToPathBufferNotLoaded :
    (B.TextBuffer -> View -> View)
    -> Buffer
    -> Buffer
    -> Editor
    -> ( Editor, Cmd Msg )
jumpToPathBufferNotLoaded updateCursor toBuf buf ed =
    let
        toBuf1 =
            Buf.updateView
                (resizeView buf.view.size
                    >> (\v1 -> { v1 | alternativeBuf = Just buf.path })
                    >> updateCursor toBuf.lines
                )
                toBuf

        global =
            ed.global

        registers =
            jumpToPathUpdateRegisters buf.path toBuf1.path ed.global.registers
    in
    ( { ed | global = { global | registers = registers } }
    , sendReadBuffer ed.global.service buf.view.size.height True toBuf1
    )


jumpToPathUpdateRegisters :
    String
    -> String
    -> Dict String RegisterText
    -> Dict String RegisterText
jumpToPathUpdateRegisters from to =
    Dict.insert "#" (Text from)
        >> Dict.insert "%" (Text to)


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

            else if newn < height then
                0

            else
                newn

        n =
            floorFromZero (toFloat height * factor)

        y =
            lineScope (Tuple.first buf.view.cursor + n)

        scrollTop =
            scrollScope view.scrollTop n
    in
    case Buf.cursorLineFirst buf.lines y of
        Just cursor ->
            buf
                |> setVisualEnd cursor
                |> Buf.updateView
                    (Buf.setCursor cursor True
                        >> Buf.setScrollTop scrollTop global.lineHeight
                    )

        Nothing ->
            buf



-- relies on jump to path


jumpToLocation :
    (View -> Win.Window View -> Win.Window View)
    -> Bool
    -> Location
    -> Editor
    -> ( Editor, Cmd Msg )
jumpToLocation setView isSaveJump { path, cursor } ed =
    jumpToPath isSaveJump path (Just cursor) setView ed


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
        |= P.loop []
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


jumpHistory : Bool -> Editor -> ( Editor, Cmd Msg )
jumpHistory isForward ({ global, buf } as ed) =
    let
        jumps =
            if isForward then
                jumpForward global.jumps

            else
                jumpBackward
                    { path = buf.path
                    , cursor = buf.view.cursor
                    }
                    global.jumps
    in
    case currentLocation jumps of
        Just loc ->
            jumpToLocation replaceActiveView
                False
                loc
                { ed | global = { global | jumps = jumps } }

        _ ->
            ( ed, Cmd.none )


jumpLastBuffer : Editor -> ( Editor, Cmd Msg )
jumpLastBuffer ({ global, buf } as ed) =
    case Win.getActiveView global.window of
        Just { alternativeBuf } ->
            case alternativeBuf of
                Just path ->
                    jumpToPath True path Nothing replaceActiveView ed

                _ ->
                    ( ed, Cmd.none )

        _ ->
            ( ed, Cmd.none )


startJumpToTag : Maybe Int -> Editor -> ( Editor, Cmd Msg )
startJumpToTag count ({ buf, global } as ed) =
    case
        wordStringUnderCursor
            buf.config.wordChars
            buf.lines
            buf.view.cursor
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
                    if buf.path == "[Search]" then
                        jumpToLocation
                            -- FIXME: activePreView will not work
                            -- when loc is a new buffer
                            (\view win ->
                                win
                                    |> Win.activePrevView
                                    |> replaceActiveView view
                            )
                            True
                            loc
                            ed

                    else
                        jumpToLocation replaceActiveView True loc ed

                _ ->
                    ( ed, Cmd.none )

        _ ->
            ( ed, Cmd.none )
