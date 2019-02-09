module Update.Jump exposing
    ( jumpByView
    , jumpHistory
    , jumpLastBuffer
    , jumpToFile
    , jumpToLocation
    , jumpToPath
    , locationParser
    , replaceActiveView
    , resizeView
    , startJumpToTag
    , tokenizeBufferCmd
    , tokenizeLineCmd
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


replaceActiveView : View -> Win.Window View -> Win.Window View
replaceActiveView view =
    Win.updateActiveView
        (\{ size } -> resizeView size view)


jumpToLocation :
    (View -> Win.Window View -> Win.Window View)
    -> Bool
    -> Location
    -> Editor
    -> ( Editor, Cmd Msg )
jumpToLocation setView isSaveJump { path, cursor } ed =
    jumpToPath isSaveJump path (Just cursor) setView ed


{-| multiple cases:

  - buffer exists
      - not loaded
      - loaded
      - same buffer
  - buffer not exists
      - temp buffer (loaded)
      - file buffer (not loaded)

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

        updateCursor : Buffer -> Buffer
        updateCursor buf1 =
            case overrideCursor of
                Just cursor ->
                    jumpToPathSetCursor global.lineHeight cursor buf1

                _ ->
                    buf1

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
    in
    case jumpToPathFindBuffer global1 path of
        -- buffer exists
        Just ( b, loaded ) ->
            if loaded then
                -- loaded
                ( if buf.id == b.id then
                    -- same buffer
                    { ed
                        | buf = updateCursor buf
                        , global = global1
                    }

                  else
                    { ed
                        | global =
                            let
                                b1 =
                                    b
                                        |> updateCursor
                                        |> Buf.updateView
                                            (\v -> { v | alternativeBuf = Just buf.path })
                            in
                            { global1
                                | buffers = Dict.insert b1.id (Loaded b1) global1.buffers
                                , window = setView b1.view global1.window
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
                  --     ]
                )

            else
                -- not loaded
                let
                    b1 =
                        b
                            |> Buf.updateView (resizeView buf.view.size)
                            |> updateCursor
                in
                ( { ed | global = global1 }
                , sendReadBuffer global1.service buf.view.size.height True b1
                )

        -- buffer not exists
        _ ->
            let
                ( global2, b ) =
                    createBuffer path buf.view.size global1
                        |> Tuple.mapSecond
                            (updateCursor
                                >> Buf.updateView
                                    (\v ->
                                        { v
                                            | alternativeBuf = Just buf.path
                                        }
                                    )
                            )
            in
            if isTempBuffer path then
                let
                    b1 =
                        Buf.updateView
                            (\v -> { v | alternativeBuf = Just buf.path })
                            b
                in
                ( { ed
                    | global =
                        { global2
                            | buffers = Dict.insert b1.id (Loaded b1) global2.buffers
                            , window =
                                setView b1.view global2.window
                        }
                  }
                , Cmd.none
                )

            else
                let
                    b1 =
                        b
                            |> Buf.updateView (resizeView buf.view.size)
                            |> updateCursor
                in
                ( { ed | global = global2 }
                , sendReadBuffer global2.service buf.view.size.height True b1
                )


jumpToPathFindBuffer : Global -> String -> Maybe ( Buffer, Bool )
jumpToPathFindBuffer global path =
    global.buffers
        |> listBuffers
        |> findFirst (\( b, _ ) -> b.path == path)


jumpToPathSetCursor : Int -> Position -> Buffer -> Buffer
jumpToPathSetCursor lineHeight cursor buf =
    let
        scrollTop =
            Buf.bestScrollTop
                (Tuple.first cursor)
                buf.view.size.height
                buf.lines
                buf.view.scrollTop
    in
    Buf.updateView
        (Buf.setCursor cursor True
            >> Buf.setScrollTop scrollTop lineHeight
        )
        buf


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


resizeView : Size -> View -> View
resizeView size view =
    if size == view.size then
        view

    else
        { view
            | size = size
            , lines =
                List.range view.scrollTop
                    (view.scrollTop + size.height + 1)
        }


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
