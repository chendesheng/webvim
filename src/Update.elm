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
            emptyExBuffer
                |> Buf.transaction
                    [ Insertion ( 0, 0 ) <|
                        B.fromString prefix
                    ]
                |> Ex prefix

        V.ModeNameVisual tipe ->
            let
                ( begin, end ) =
                    case mode of
                        Visual _ a b ->
                            ( a, b )

                        _ ->
                            ( cursor, cursor )
            in
                case tipe of
                    V.VisualNameLine ->
                        Visual VisualLine begin end

                    V.VisualNameBlock ->
                        Visual VisualBlock begin end

                    _ ->
                        Visual VisualRange begin end


getModeName : Mode -> V.ModeName
getModeName mode =
    case mode of
        Normal ->
            V.ModeNameNormal

        Insert ->
            V.ModeNameInsert

        TempNormal ->
            V.ModeNameTempNormal

        Visual tipe _ _ ->
            V.ModeNameVisual
                (case tipe of
                    VisualRange ->
                        V.VisualName

                    VisualLine ->
                        V.VisualNameLine

                    VisualBlock ->
                        V.VisualNameBlock
                )

        Ex prefix _ ->
            V.ModeNameEx prefix


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


modeChanged : V.ModeName -> V.ModeName -> Buffer -> Buffer
modeChanged oldModeName newModeName buf =
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
                        , if Buf.getLineMaxColumn y buf.lines > x then
                            x
                          else
                            max (x - 1) 0
                        )
            in
                buf
                    |> Buf.setCursor cursor (oldModeName == V.ModeNameInsert)
                    |> Buf.commit

        V.ModeNameTempNormal ->
            buf
                |> Buf.commit

        V.ModeNameEx prefix ->
            case buf.mode of
                Ex prefix exbuf ->
                    if B.isEmpty exbuf.lines then
                        buf
                            |> handleKeypress False "<esc>"
                            |> Tuple.first
                    else
                        buf

                _ ->
                    buf

        _ ->
            buf


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
                            Ex prefix exbuf ->
                                Buf.putString forward s exbuf
                                    |> Ex prefix
                                    |> flip Buf.setMode buf

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
                Visual tipe begin end ->
                    { buf
                        | mode = Visual tipe end begin
                        , cursor = begin
                        , cursorColumn = Tuple.second begin
                    }

                _ ->
                    buf

        _ ->
            buf


{-| scroll cursor ensure it is insdie viewport
-}
scrollToCursor : Buffer -> Buffer
scrollToCursor ({ view, cursor, lines } as buf) =
    let
        ( y, _ ) =
            cursor

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


{-| move cursor ensure cursor is insdie viewport
-}
cursorScope : Buffer -> Buffer
cursorScope ({ view, cursor, lines } as buf) =
    let
        ( y, _ ) =
            cursor

        miny =
            view.scrollTop

        maxy =
            min
                (miny + view.size.height - 1)
                (B.countLines lines - 1)

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
            -- for now the put operator is implemented as
            -- Start insert mode -> Put string -> Back to normal mode
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

        saveLastVisual buf =
            if replaying || key == "<visual>" then
                buf
            else
                let
                    last =
                        buf.last

                    last1 =
                        if
                            (isModeNameVisual oldModeName)
                                && (isModeNameVisual modeName)
                        then
                            { last | visual = last.visual ++ key }
                        else if
                            (isModeNameVisual oldModeName |> not)
                                && (isModeNameVisual modeName)
                        then
                            { last | visual = "" }
                        else
                            last
                in
                    { buf | last = last1 }

        saveLastInsert buf =
            if replaying || key == "<inserts>" then
                buf
            else
                let
                    last =
                        buf.last

                    last1 =
                        if
                            (oldModeName == V.ModeNameInsert)
                                && (modeName == V.ModeNameInsert)
                        then
                            { last | inserts = last.inserts ++ key }
                        else if
                            (oldModeName == V.ModeNameNormal)
                                && (modeName == V.ModeNameInsert)
                                || (oldModeName == V.ModeNameTempNormal)
                                && (modeName == V.ModeNameInsert)
                        then
                            { last | inserts = "" }
                        else
                            last
                in
                    { buf | last = last1 }
    in
        ( buf
            |> setContinuation continuation
            |> applyEdit edit register
            |> updateMode modeName
            |> modeChanged oldModeName modeName
            |> scrollToCursor
            |> saveLastInsert
            |> saveLastVisual
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
