module Update exposing (update)

import Model exposing (..)
import Message exposing (..)
import Vim.Parser as P
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Buffer as Buf
import Position exposing (Position, positionMin)
import String
import PositionClass exposing (..)


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


moveByClass : V.MotionData -> V.MotionOption -> Buffer -> Position
moveByClass class option buf =
    let
        ( y, x ) =
            buf.cursor
    in
        case class of
            V.CharStart ->
                let
                    newx =
                        if option.forward then
                            x + 1
                        else
                            x - 1
                in
                    if option.crossLine then
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
                                    option
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
        V.MotionRange md mo ->
            let
                pos =
                    runMotion md mo buf

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
                        , if mo.inclusive then
                            endx + 1
                          else
                            endx
                        )
            in
                buf
                    |> Buf.transaction [ patch ]

        _ ->
            buf


matchChar :
    String
    -> V.MotionOption
    -> Buffer
    -> Position
matchChar char mo buf =
    let
        ( y, x ) =
            buf.cursor
    in
        buf.lines
            |> B.getLine y
            |> Maybe.map
                (\line ->
                    if mo.forward then
                        line
                            |> String.dropLeft (x + 1)
                            |> String.indexes char
                            |> List.head
                            |> Maybe.map
                                ((+)
                                    (if mo.inclusive then
                                        x + 1
                                     else
                                        x
                                    )
                                )
                            |> Maybe.withDefault x
                    else
                        line
                            |> String.left x
                            |> String.indexes char
                            |> getLast
                            |> Maybe.map
                                ((+)
                                    (if mo.inclusive then
                                        0
                                     else
                                        1
                                    )
                                )
                            |> Maybe.withDefault x
                )
            |> Maybe.map (\x1 -> ( y, x1 ))
            |> Maybe.withDefault buf.cursor


runMotion : V.MotionData -> V.MotionOption -> Buffer -> Position
runMotion md mo buf =
    if B.isEmpty buf.lines then
        ( 0, 0 )
    else
        let
            ( y, x ) =
                buf.cursor
        in
            case md of
                V.MatchChar ch ->
                    matchChar ch mo buf

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
                    moveByClass md mo buf


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
            emptyExBuffer
                |> Buf.transaction [ Insertion ( 0, 0 ) <| B.fromString prefix ]
                |> Ex prefix

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
                        , if getLineMaxColumn y buf.lines > x then
                            x
                          else
                            max (x - 1) 0
                        )
            in
                buf
                    |> setCursor cursor
                    |> Buf.commit

        V.ModeNameTempNormal ->
            Buf.commit buf

        V.ModeNameEx prefix ->
            case buf.mode of
                Ex prefix exbuf ->
                    if B.isEmpty exbuf.lines then
                        buf
                            |> handleKeypress "<esc>"
                            |> Tuple.first
                    else
                        buf

                _ ->
                    buf

        _ ->
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


insertString : V.StringType -> Buffer -> Buffer
insertString ins buf =
    let
        s =
            getString buf ins

        { expandTab, tabSize } =
            buf.config

        s1 =
            if expandTab then
                B.fromStringExpandTabs
                    tabSize
                    (buf.cursor
                        |> Tuple.second
                    )
                    s
            else
                B.fromString s
    in
        Buf.transaction
            [ Insertion buf.cursor s1 ]
            buf


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


setContinuation : String -> Buffer -> Buffer
setContinuation s buf =
    { buf | continuation = s }


getString : Buffer -> V.StringType -> String
getString buf ins =
    case ins of
        V.TextLiteral s ->
            s

        _ ->
            ""


runOperator : Operator -> Buffer -> Buffer
runOperator operator buf =
    case operator of
        Move md mo ->
            buf
                |> setCursor (runMotion md mo buf)

        InsertString s ->
            case buf.mode of
                Ex prefix exbuf ->
                    { buf
                        | mode =
                            exbuf
                                |> insertString s
                                |> Ex prefix
                    }

                _ ->
                    insertString s buf

        Delete rg ->
            case buf.mode of
                Ex prefix exbuf ->
                    { buf
                        | mode =
                            exbuf
                                |> deleteOperator rg
                                |> Ex prefix
                    }

                _ ->
                    deleteOperator rg buf

        Undo ->
            Buf.undo buf

        Redo ->
            Buf.redo buf

        OpenNewLine V.Forward ->
            openNewLine (Tuple.first buf.cursor + 1) buf

        OpenNewLine V.Backward ->
            openNewLine (Tuple.first buf.cursor) buf

        _ ->
            buf


handleKeypress : Key -> Buffer -> ( Buffer, Cmd Msg )
handleKeypress key buf =
    let
        oldModeName =
            getModeName buf.mode

        ( { edit, modeName } as ast, continuation ) =
            P.parse buf.continuation key

        applyEdit b =
            edit
                |> Maybe.map (flip runOperator b)
                |> Maybe.withDefault b
    in
        ( buf
            |> updateMode modeName
            |> setContinuation continuation
            |> applyEdit
            |> modeChanged oldModeName modeName
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        PressKey bufid key ->
            handleKeypress key model
