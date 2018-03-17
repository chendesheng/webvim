module Update exposing (update)

import Model exposing (..)
import Message exposing (..)
import Vim.Parser as P
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Buffer as Buf
import Position exposing (Position, positionMin)
import Dict


getLineMaxCursor : Int -> B.TextBuffer -> Int
getLineMaxCursor y lines =
    B.getLine y lines
        |> Maybe.map
            (\s ->
                let
                    len =
                        String.length s
                in
                    if String.endsWith B.lineBreak s then
                        len - 2
                    else
                        len - 1
            )
        |> Maybe.withDefault 0


moveByClass : V.PositionClass -> V.Direction -> Buffer -> Position
moveByClass class direction buf =
    let
        ( y, x ) =
            buf.cursor
    in
        case class of
            V.CharStart crossLine ->
                let
                    newx =
                        if direction == V.Forward then
                            x + 1
                        else
                            x - 1
                in
                    if crossLine then
                        if newx < 0 then
                            if y > 0 then
                                ( y - 1
                                , B.getLine (y - 1) buf.lines
                                    |> Maybe.map (\s -> String.length s - 1)
                                    |> Maybe.withDefault 0
                                )
                            else
                                ( 0, 0 )
                        else
                            ( y, newx )
                    else
                        ( y, min (max newx 0) (getLineMaxCursor y buf.lines) )

            _ ->
                buf.cursor


deleteOperator : Buffer -> V.OperatorRange -> Buffer
deleteOperator buf range =
    case range of
        V.MotionRange inclusive motion ->
            let
                pos =
                    runMotion motion buf

                begin =
                    if pos > buf.cursor then
                        buf.cursor
                    else
                        pos

                ( endy, endx ) =
                    if pos > buf.cursor then
                        pos
                    else
                        buf.cursor

                patch =
                    Deletion begin
                        ( endy
                        , case inclusive of
                            V.Inclusive ->
                                endx + 1

                            V.Exclusive ->
                                endx
                        )
            in
                Buf.transaction [ patch ] buf

        _ ->
            buf


runMotion : V.Motion -> Buffer -> Position
runMotion motion buf =
    if B.isEmpty buf.lines then
        ( 0, 0 )
    else
        let
            ( y, x ) =
                buf.cursor
        in
            case motion of
                V.ByClass { class, direction } ->
                    moveByClass class direction buf

                V.LineDelta n ->
                    let
                        y1 =
                            (y + n)
                                |> max 0
                                |> min (B.countLines buf.lines - 1)

                        x1 =
                            buf.lines
                                |> getLineMaxCursor y1
                                |> min x
                    in
                        ( y1, x1 )

                _ ->
                    buf.cursor


emptyMode : V.ModeName -> Mode
emptyMode modeName =
    case modeName of
        V.ModeNameNormal ->
            Normal

        V.ModeNameTempNormal ->
            TempNormal

        V.ModeNameInsert ->
            Insert

        V.ModeNameEx prefix ->
            Ex
                { prefix = prefix
                , buffer = ""
                , cursor = ( 0, 0 )
                }

        V.ModeNameVisual _ ->
            Visual VisualRange []


getModeName : Mode -> V.ModeName
getModeName mode =
    case mode of
        Normal ->
            V.ModeNameNormal

        Insert ->
            V.ModeNameInsert

        TempNormal ->
            V.ModeNameTempNormal

        Visual tipe _ ->
            V.ModeNameVisual
                (case tipe of
                    VisualRange ->
                        V.VisualName

                    VisualLine ->
                        V.VisualNameLine

                    VisualBlock ->
                        V.VisualNameBlock
                )

        Ex _ ->
            V.ModeNameEx ""


setCursor : Position -> Buffer -> Buffer
setCursor cursor buf =
    { buf | cursor = cursor }


updateMode : V.ModeName -> Buffer -> Buffer
updateMode modeName buf =
    let
        oldModeName =
            getModeName buf.mode

        newMode =
            if oldModeName == modeName then
                buf.mode
            else
                emptyMode modeName
    in
        { buf | mode = newMode }


modeChanged : V.ModeName -> V.ModeName -> Buffer -> Buffer
modeChanged oldModeName newModeName buf =
    if
        oldModeName
            == V.ModeNameInsert
            && newModeName
            == V.ModeNameNormal
    then
        let
            ( y, x ) =
                buf.cursor
        in
            buf
                |> setCursor ( y, max (x - 1) 0 )
                |> Buf.commit
    else if newModeName == V.ModeNameNormal then
        Buf.commit buf
    else
        buf


getLast : List a -> Maybe a
getLast xs =
    case xs of
        [] ->
            Nothing

        [ x ] ->
            Just x

        x :: xs ->
            getLast xs


handleKeypress : Key -> Model -> Buffer -> ( Model, Cmd Msg )
handleKeypress key model buf =
    let
        oldModeName =
            getModeName buf.mode

        ( { edit, modeName }, continuation ) =
            P.parse buf.continuation key

        buf1 =
            updateMode modeName buf

        buf2 =
            edit
                |> Maybe.map
                    (\operator ->
                        case operator of
                            Move motion ->
                                buf1
                                    |> setCursor (runMotion motion buf1)

                            InsertString s ->
                                case buf1.mode of
                                    Ex linebuf ->
                                        buf1

                                    _ ->
                                        let
                                            { expandTab, tabSize } =
                                                buf1.config

                                            s1 =
                                                if expandTab then
                                                    B.fromStringExpandTabs
                                                        tabSize
                                                        (buf1.cursor
                                                            |> Tuple.second
                                                        )
                                                        s
                                                else
                                                    B.fromString s
                                        in
                                            Buf.transaction
                                                [ Insertion buf1.cursor s1 ]
                                                buf1

                            Delete rg ->
                                deleteOperator buf1 rg

                            _ ->
                                buf1
                    )
                |> Maybe.withDefault buf1

        buf3 =
            if oldModeName == modeName then
                buf2
            else
                modeChanged oldModeName modeName buf2

        buffers =
            Dict.insert buf3.id
                { buf3 | continuation = continuation }
                model.buffers
    in
        ( { model | buffers = buffers }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        PressKey bufid key ->
            model
                |> getBuffer bufid
                |> Maybe.map (handleKeypress key model)
                |> Maybe.withDefault ( model, Cmd.none )
