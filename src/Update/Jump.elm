module Update.Jump exposing
    ( jumpByView
    , jumpHistory
    , jumpLastBuffer
    , jumpToFile
    , jumpToLocation
    , jumpToPath
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
import Model.Buffer exposing (..)
import Model.Frame as Frame exposing (Frame)
import Model.Global exposing (..)
import Model.View as View exposing (View, emptyView, resizeView)
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


getViewFromActiveFrame : Win.Window Frame -> String -> View
getViewFromActiveFrame window bufId =
    let
        frame =
            Win.getActiveFrame window
                |> Maybe.withDefault Frame.empty
    in
    frame
        |> Frame.getView bufId
        |> Maybe.withDefault (Frame.newView bufId frame)


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

setup window:

  - update window
  - insert into buffers

-}
jumpToPath :
    Bool
    -> String
    -> Maybe Position
    -> Editor
    -> ( Editor, Cmd Msg )
jumpToPath isSaveJump path_ overrideCursor ({ global, buf } as ed) =
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

        global1 =
            if isSaveJump then
                updateJumps
                    (saveJump
                        { path = buf.path
                        , cursor = buf.view.cursor
                        }
                    )
                    global

            else
                global

        ed1 =
            { ed | global = global1 }
    in
    case jumpToPathFindBuffer global1 path of
        -- buffer exists
        Just ( b, loaded ) ->
            if loaded then
                -- loaded
                jumpToPathBufferLoaded overrideCursor b buf ed1

            else
                -- not loaded
                jumpToPathBufferNotLoaded overrideCursor b buf ed1

        -- buffer not exists
        _ ->
            let
                ( global2, b ) =
                    createBuffer path global1

                ed2 =
                    { ed1 | global = global2 }
            in
            if isTempBuffer path then
                -- loaded
                jumpToPathBufferLoaded overrideCursor b buf ed2

            else
                -- not loaded
                jumpToPathBufferNotLoaded overrideCursor b buf ed2


jumpToPathFindBuffer : Global -> String -> Maybe ( Buffer, Bool )
jumpToPathFindBuffer global path =
    global.buffers
        |> listBuffers
        |> findFirst (\( b, _ ) -> b.path == path)


jumpToPathSetCursor : Maybe Position -> View -> View
jumpToPathSetCursor overrideCursor view =
    case overrideCursor of
        Just cursor ->
            let
                scrollTop =
                    Buf.bestScrollTop
                        (Tuple.first cursor)
                        view.size.height
                        view.scrollTop
            in
            view
                |> View.setCursor cursor True
                |> View.setScrollTop scrollTop

        _ ->
            view


jumpToPathBufferLoaded : Maybe Position -> Buffer -> Buffer -> Editor -> ( Editor, Cmd Msg )
jumpToPathBufferLoaded overrideCursor toBuf buf ed =
    if buf.id == toBuf.id then
        -- same buffer
        ( { ed
            | buf =
                Buf.updateView
                    (jumpToPathSetCursor overrideCursor)
                    buf
          }
        , Cmd.none
        )

    else
        let
            global =
                ed.global

            view =
                getViewFromActiveFrame global.window toBuf.id
                    |> jumpToPathSetCursor overrideCursor

            window =
                Win.updateActiveFrame (Frame.addOrActiveView view) global.window
        in
        ( { ed | global = { global | window = window } }, Cmd.none )


jumpToPathBufferNotLoaded : Maybe Position -> Buffer -> Buffer -> Editor -> ( Editor, Cmd Msg )
jumpToPathBufferNotLoaded overrideCursor toBuf buf ({ global } as ed) =
    let
        toBuf1 =
            { toBuf
                | view =
                    getViewFromActiveFrame ed.global.window toBuf.id
                        |> jumpToPathSetCursor overrideCursor
            }
    in
    ( ed, sendReadBuffer global.service buf.view.size.height global.window.path toBuf1 )


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
                    (View.setCursor cursor True
                        >> View.setScrollTop scrollTop
                    )

        Nothing ->
            buf



-- relies on jump to path


jumpToLocation :
    Bool
    -> Location
    -> Editor
    -> ( Editor, Cmd Msg )
jumpToLocation isSaveJump { path, cursor } ed =
    jumpToPath isSaveJump path (Just cursor) ed


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
        global1 =
            global
                |> updateJumps
                    (\jumps ->
                        if isForward then
                            jumpForward jumps

                        else
                            jumpBackward
                                { path = buf.path
                                , cursor = buf.view.cursor
                                }
                                jumps
                    )
    in
    case currentLocation <| getJumps global1 of
        Just loc ->
            jumpToLocation
                False
                loc
                { ed | global = global1 }

        _ ->
            ( ed, Cmd.none )


jumpLastBuffer : Editor -> ( Editor, Cmd Msg )
jumpLastBuffer ({ global, buf } as ed) =
    global.window
        |> Win.getActiveFrame
        |> Maybe.andThen Frame.getAlterViewId
        |> Maybe.map
            (\path ->
                jumpToPath True path Nothing ed
            )
        |> Maybe.withDefault
            ( { ed
                | buf = Buf.errorMessage "No alternate file" buf
              }
            , Cmd.none
            )


activePrevView : Editor -> Editor
activePrevView ({ global } as ed) =
    { ed
        | global =
            { global | window = Win.activePrevFrame global.window }
    }


jumpToFile : Editor -> ( Editor, Cmd Msg )
jumpToFile ({ buf } as ed) =
    case wORDStringUnderCursor buf of
        Just ( _, s ) ->
            case P.run locationParser s of
                Ok loc ->
                    if buf.path == "[Search]" then
                        jumpToLocation
                            -- FIXME: activePrevView will not work
                            -- when loc is a new buffer
                            False
                            loc
                            (activePrevView ed)

                    else
                        jumpToLocation True loc ed

                _ ->
                    ( ed, Cmd.none )

        _ ->
            ( ed, Cmd.none )
