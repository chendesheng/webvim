module Update exposing (update)

import Model exposing (..)
import Message exposing (..)
import Vim.Parser as P
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Buffer as Buf
import Position exposing (Position, positionMin)
import String
import PositionClass exposing (findPosition)


getLineMaxColumn : Int -> B.TextBuffer -> Int
getLineMaxColumn y lines =
    B.getLine y lines
        |> Maybe.map
            (\s ->
                let
                    len =
                        String.length s
                in
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
                        ( y, min (max newx 0) (getLineMaxColumn y buf.lines) )

            class ->
                buf.lines
                    |> B.getLine y
                    |> Maybe.map
                        (\line ->
                            case
                                (findPosition
                                    buf.config.wordChars
                                    class
                                    direction
                                    line
                                    x
                                )
                            of
                                Just x1 ->
                                    ( y, x1 )

                                Nothing ->
                                    buf.cursor
                        )
                    |> Maybe.withDefault buf.cursor


deleteOperator : V.OperatorRange -> Buffer -> Buffer
deleteOperator range buf =
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
                buf
                    |> Buf.transaction [ patch ]

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
                                |> getLineMaxColumn y1
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
            Ex prefix emptyExBuffer

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

        Ex prefix _ ->
            V.ModeNameEx prefix


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
    if newModeName == V.ModeNameNormal then
        let
            ( y, x ) =
                buf.cursor

            buf1 =
                if oldModeName == V.ModeNameInsert then
                    setCursor ( y, max (x - 1) 0 ) buf
                else
                    setCursor
                        ( y
                        , if getLineMaxColumn y buf.lines > x then
                            x
                          else
                            max (x - 1) 0
                        )
                        buf
        in
            Buf.commit buf1
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


insertString : String -> Buffer -> Buffer
insertString s buf1 =
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


openNewLine : Int -> Buffer -> Buffer
openNewLine y buf =
    let
        n =
            B.countLines buf.lines

        cursor =
            ( y
                |> max 0
                |> min n
            , 0
            )

        patch =
            Insertion cursor <| B.fromString B.lineBreak
    in
        buf
            |> Buf.transaction [ patch ]
            |> setCursor cursor


handleKeypress : Key -> Buffer -> ( Buffer, Cmd Msg )
handleKeypress key buf =
    let
        oldModeName =
            getModeName buf.mode

        ( { edit, modeName } as ast, continuation ) =
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
                                    Ex prefix buf ->
                                        { buf1
                                            | mode =
                                                buf
                                                    |> insertString s
                                                    |> Ex prefix
                                        }

                                    _ ->
                                        insertString s buf1

                            Delete rg ->
                                case buf1.mode of
                                    Ex prefix buf ->
                                        { buf1
                                            | mode =
                                                buf
                                                    |> deleteOperator rg
                                                    |> Ex prefix
                                        }

                                    _ ->
                                        deleteOperator rg buf1

                            Undo ->
                                Buf.undo buf1

                            Redo ->
                                Buf.redo buf1

                            OpenNewLine V.Forward ->
                                openNewLine (Tuple.first buf1.cursor + 1) buf1

                            OpenNewLine V.Backward ->
                                openNewLine (Tuple.first buf1.cursor) buf1

                            _ ->
                                buf1
                    )
                |> Maybe.withDefault buf1

        buf3 =
            modeChanged oldModeName modeName buf2
    in
        ( { buf3
            | continuation = continuation
          }
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        PressKey bufid key ->
            handleKeypress key model
