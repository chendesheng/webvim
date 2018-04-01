module Update exposing (update)

import Model exposing (..)
import Message exposing (..)
import Vim.Helper exposing (keyParser, escapeKey)
import Vim.Parser exposing (parse)
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Buffer as Buf
import Dict
import Parser as P exposing ((|.), (|=), Parser)
import Motion exposing (..)
import Delete exposing (..)
import Insert exposing (..)
import Position exposing (Position)
import Regex as Re


stringToPrefix : String -> ExPrefix
stringToPrefix prefix =
    case prefix of
        "/" ->
            ExSearch { forward = True, match = Nothing }

        "?" ->
            ExSearch { forward = False, match = Nothing }

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
            Normal

        V.ModeNameTempNormal ->
            TempNormal

        V.ModeNameInsert ->
            Insert

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
        Normal ->
            V.ModeNameNormal

        Insert ->
            V.ModeNameInsert

        TempNormal ->
            V.ModeNameTempNormal

        Visual { tipe } ->
            V.ModeNameVisual tipe

        Ex { prefix } ->
            V.ModeNameEx <| prefixToString prefix


updateMode : V.ModeName -> Buffer -> Buffer
updateMode modeName buf =
    let
        oldModeName =
            getModeName buf.mode

        newMode =
            if oldModeName == modeName then
                buf.mode
            else
                initMode buf modeName
    in
        { buf | mode = newMode }


modeChanged : Bool -> Key -> V.ModeName -> V.ModeName -> Buffer -> Buffer
modeChanged replaying key oldModeName newModeName buf =
    case newModeName of
        V.ModeNameNormal ->
            let
                ( y, x ) =
                    buf.cursor

                cursor =
                    if oldModeName == V.ModeNameInsert then
                        ( y, max (x - 1) 0 )
                    else
                        ( y
                        , if B.getLineMaxColumn y buf.lines > x then
                            x
                          else
                            max (x - 1) 0
                        )
            in
                buf
                    |> Buf.setCursor
                        cursor
                        (oldModeName == V.ModeNameInsert)
                    |> Buf.commit

        V.ModeNameTempNormal ->
            buf
                |> Buf.commit

        V.ModeNameEx prefix ->
            case buf.mode of
                Ex ({ prefix, exbuf } as ex) ->
                    if B.isEmpty exbuf.lines then
                        buf
                            |> handleKeypress False "<esc>"
                            |> Tuple.first
                    else
                        let
                            prefix1 =
                                case prefix of
                                    ExSearch ({ forward } as search) ->
                                        let
                                            s =
                                                exbuf.lines
                                                    |> B.toString
                                                    |> String.dropLeft 1

                                            re =
                                                Re.regex s
                                        in
                                            if String.isEmpty s then
                                                ExSearch
                                                    { search | match = Nothing }
                                            else
                                                ExSearch
                                                    { search
                                                        | match =
                                                            matchString forward
                                                                re
                                                                buf.cursor
                                                                buf.lines
                                                    }

                                    _ ->
                                        prefix
                        in
                            Buf.setMode
                                (Ex { ex | prefix = prefix1 })
                                buf

                _ ->
                    buf

        V.ModeNameInsert ->
            let
                last =
                    buf.last

                inserts =
                    if replaying || key == "<inserts>" then
                        last.inserts
                    else
                        case oldModeName of
                            V.ModeNameInsert ->
                                last.inserts ++ key

                            _ ->
                                ""
            in
                { buf | last = { last | inserts = inserts } }

        V.ModeNameVisual _ ->
            let
                last =
                    buf.last

                visual =
                    if replaying || key == "<visual>" then
                        last.visual
                    else
                        case oldModeName of
                            V.ModeNameVisual _ ->
                                last.visual ++ key

                            _ ->
                                ""
            in
                { buf | last = { last | visual = visual } }


setContinuation : String -> Buffer -> Buffer
setContinuation s buf =
    { buf | continuation = s }


scrollToLine : Int -> Buffer -> Buffer
scrollToLine n buf =
    updateView (\v -> { v | scrollTop = n }) buf


ceilingFromZero : Float -> Int
ceilingFromZero n =
    if n < 0 then
        floor n
    else
        ceiling n


runOperator : String -> Operator -> Buffer -> Buffer
runOperator register operator buf =
    case operator of
        Move md mo ->
            motion md mo buf

        Scroll value ->
            let
                scrollTop =
                    let
                        y =
                            Tuple.first buf.cursor
                    in
                        case value of
                            V.ScrollBy n ->
                                buf.view.scrollTop + n

                            V.ScrollToTop ->
                                y

                            V.ScrollToBottom ->
                                y - buf.view.size.height + 1

                            V.ScrollToMiddle ->
                                y - buf.view.size.height // 2

                scope n =
                    n
                        |> max 0
                        |> min (B.countLines buf.lines - 1)
            in
                buf
                    |> scrollToLine (scope scrollTop)
                    |> cursorScope

        InsertString s ->
            insert s buf

        Delete rg ->
            delete register rg buf
                |> cursorScope

        Undo ->
            Buf.undo buf

        Redo ->
            Buf.redo buf

        OpenNewLine V.Forward ->
            openNewLine (Tuple.first buf.cursor + 1) buf

        OpenNewLine V.Backward ->
            openNewLine (Tuple.first buf.cursor) buf

        JumpByView factor ->
            let
                view =
                    buf.view

                height =
                    view.size.height

                lineScope row =
                    row
                        |> max 0
                        |> min (B.countLines buf.lines - 1)

                scrollScope scrollTop n =
                    let
                        newn =
                            scrollTop + n

                        maxy =
                            B.countLines buf.lines - 1
                    in
                        if newn < 0 then
                            scrollTop
                        else if (newn + height) > maxy then
                            max (maxy - height + 1) 0
                        else
                            newn

                n =
                    ceilingFromZero (toFloat height * factor)

                y =
                    lineScope (Tuple.first buf.cursor + n)

                scrollTop =
                    scrollScope view.scrollTop n
            in
                case gotoLine y buf.lines of
                    Just cursor ->
                        buf
                            |> Buf.setCursor cursor True
                            |> setVisualEnd cursor
                            |> scrollToLine scrollTop

                    Nothing ->
                        buf

        Put forward ->
            Dict.get register buf.registers
                |> Maybe.map
                    (\s ->
                        case buf.mode of
                            Ex ({ exbuf } as ex) ->
                                Buf.setMode
                                    (Ex
                                        { ex
                                            | exbuf =
                                                Buf.putString forward s exbuf
                                        }
                                    )
                                    buf

                            _ ->
                                Buf.putString forward s buf
                    )
                |> Maybe.withDefault buf

        RepeatLastOperator ->
            case Dict.get "." buf.registers of
                Just keys ->
                    buf
                        |> replayKeys keys

                _ ->
                    buf

        RepeatLastInsert ->
            replayKeys buf.last.inserts buf

        RepeatLastVisual ->
            replayKeys buf.last.visual buf

        VisualSwitchEnd ->
            case buf.mode of
                Visual { tipe, begin, end } ->
                    { buf
                        | mode =
                            Visual
                                { tipe = tipe
                                , begin = end
                                , end = begin
                                }
                        , cursor = begin
                        , cursorColumn = Tuple.second begin
                    }

                _ ->
                    buf

        Execute ->
            buf

        _ ->
            buf


{-| scroll to ensure pos it is insdie viewport
-}
scrollTo : Position -> Buffer -> Buffer
scrollTo pos ({ view, lines } as buf) =
    let
        ( y, _ ) =
            pos

        miny =
            view.scrollTop

        maxy =
            miny + view.size.height - 1

        scrollTop =
            if miny > y then
                y
            else if y > maxy then
                y - maxy + miny
            else
                buf.view.scrollTop
    in
        scrollToLine scrollTop buf


scrollToCursor : Buffer -> Buffer
scrollToCursor buf =
    scrollTo buf.cursor buf


{-| move cursor ensure cursor is insdie viewport
-}
cursorScope : Buffer -> Buffer
cursorScope ({ view, cursor, lines } as buf) =
    let
        ( y, _ ) =
            cursor

        maxy =
            min
                (view.scrollTop + view.size.height - 1)
                (B.countLines lines - 1)

        miny =
            min view.scrollTop maxy

        y1 =
            y |> min maxy |> max miny
    in
        if y == y1 then
            buf
        else
            case gotoLine y1 lines of
                Just cursor ->
                    buf
                        |> Buf.setCursor cursor True
                        |> setVisualEnd cursor

                _ ->
                    buf


applyEdit : Maybe Operator -> String -> Buffer -> Buffer
applyEdit edit register buf =
    case edit of
        Just operator ->
            runOperator register operator buf

        Nothing ->
            buf


isPutOperator : Maybe Operator -> Bool
isPutOperator edit =
    case edit of
        Just operator ->
            case operator of
                Put _ ->
                    True

                _ ->
                    False

        _ ->
            False


replayKeys : String -> Model -> Model
replayKeys s buf =
    let
        savedLast =
            buf.last

        savedRegisters =
            buf.registers

        keys =
            s
                |> P.run (P.repeat P.zeroOrMore keyParser)
                |> Result.withDefault []

        buf1 =
            List.foldl
                (\key buf ->
                    handleKeypress True key buf
                        |> Tuple.first
                )
                buf
                keys
    in
        { buf1
            | last = savedLast
            , registers = savedRegisters
        }


isModeNameVisual : V.ModeName -> Bool
isModeNameVisual name =
    case name of
        V.ModeNameVisual _ ->
            True

        _ ->
            False


handleKeypress : Bool -> Key -> Buffer -> ( Buffer, Cmd Msg )
handleKeypress replaying key buf =
    let
        ( { edit, modeName, register, recordKeys } as ast, continuation ) =
            parse buf.continuation key

        -- |> Debug.log key
        oldModeName =
            -- For now the put operator is implemented as
            --   1) Start insert mode
            --   2) Put string
            --   3) Back to normal mode
            if modeName == V.ModeNameNormal && isPutOperator edit then
                V.ModeNameInsert
            else
                getModeName buf.mode

        saveDotRegister buf =
            if replaying then
                buf
            else
                case recordKeys of
                    "" ->
                        buf

                    s ->
                        Buf.setRegister "." s buf
    in
        ( buf
            |> setContinuation continuation
            |> applyEdit edit register
            |> updateMode modeName
            |> modeChanged replaying key oldModeName modeName
            |> scrollToCursor
            |> saveDotRegister
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        PressKey bufid key ->
            handleKeypress False key model

        Resize size ->
            let
                h =
                    (size.height // 21) - model.view.statusbarHeight

                w =
                    size.width
            in
                ( model
                    |> updateView
                        (\view ->
                            { view | size = { width = w, height = h } }
                        )
                    |> cursorScope
                , Cmd.none
                )
