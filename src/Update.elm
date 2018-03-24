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


deleteOperator : V.OperatorRange -> Buffer -> Buffer
deleteOperator range buf =
    case range of
        V.MotionRange md mo ->
            case runMotion md mo buf of
                Just pos ->
                    let
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
                    in
                        if mo.linewise then
                            buf
                                |> setCursor pos False
                                |> Buf.transaction
                                    [ Deletion ( Tuple.first begin, 0 )
                                        ( if mo.inclusive then
                                            endy + 1
                                          else
                                            endy
                                        , 0
                                        )
                                    ]
                                |> boundCursor
                        else
                            Buf.transaction
                                [ Deletion begin
                                    ( endy
                                    , if mo.inclusive then
                                        endx + 1
                                      else
                                        endx
                                    )
                                ]
                                buf

                Nothing ->
                    buf

        _ ->
            buf


matchChar :
    String
    -> Bool
    -> V.MotionOption
    -> Buffer
    -> Position
matchChar char before mo buf =
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
                                    (if before then
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
                                    (if before then
                                        0
                                     else
                                        1
                                    )
                                )
                            |> Maybe.withDefault x
                )
            |> Maybe.map (\x1 -> ( y, x1 ))
            |> Maybe.withDefault buf.cursor


findPositionInBuffer :
    V.MotionData
    -> V.MotionOption
    -> Int
    -> Int
    -> String
    -> String
    -> B.TextBuffer
    -> Maybe Position
findPositionInBuffer md mo y x_ pline wordChars lines =
    case B.getLine y lines of
        Just line_ ->
            let
                line =
                    if mo.forward then
                        pline ++ line_
                    else
                        line_ ++ pline

                x =
                    if mo.forward then
                        x_
                    else if String.isEmpty pline then
                        x_
                    else
                        x_ + String.length line_
            in
                case findPosition wordChars md mo line x of
                    Just x1 ->
                        Just
                            ( y
                            , if mo.forward then
                                x1 - String.length pline
                              else
                                x1
                            )

                    Nothing ->
                        if mo.crossLine then
                            if mo.forward then
                                findPositionInBuffer
                                    md
                                    mo
                                    (y + 1)
                                    x
                                    line
                                    wordChars
                                    lines
                            else
                                findPositionInBuffer
                                    md
                                    mo
                                    (y - 1)
                                    x
                                    line
                                    wordChars
                                    lines
                        else
                            Nothing

        _ ->
            Nothing


gotoLine : Int -> B.TextBuffer -> Maybe Position
gotoLine y lines =
    B.getLine y lines
        |> Maybe.andThen
            (\line ->
                (findPosition
                    ""
                    V.LineFirst
                    (V.motionOption "<]$-")
                    line
                    0
                )
                    |> Maybe.map ((,) y)
            )


runMotion : V.MotionData -> V.MotionOption -> Buffer -> Maybe Position
runMotion md mo buf =
    if B.isEmpty buf.lines then
        Nothing
    else
        let
            ( y, x ) =
                buf.cursor

            bottomLine buf =
                (min
                    (B.countLines buf.lines)
                    (buf.view.scrollTop + buf.view.size.height)
                )
                    - 1

            middleLine buf =
                (bottomLine buf + buf.view.scrollTop) // 2
        in
            case md of
                V.LineNumber n ->
                    gotoLine (n % (B.countLines buf.lines)) buf.lines

                V.LineDelta n ->
                    let
                        y1 =
                            (y + n)
                                |> max 0
                                |> min (B.countLines buf.lines - 1)

                        x1 =
                            buf.lines
                                |> getLineMaxColumn y1
                                |> min buf.cursorColumn
                    in
                        Just ( y1, x1 )

                V.ViewTop ->
                    gotoLine buf.view.scrollTop buf.lines

                V.ViewMiddle ->
                    gotoLine
                        (middleLine buf)
                        buf.lines

                V.ViewBottom ->
                    gotoLine
                        (bottomLine buf)
                        buf.lines

                _ ->
                    findPositionInBuffer md
                        mo
                        y
                        x
                        ""
                        buf.config.wordChars
                        buf.lines


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
                |> Buf.transaction
                    [ Insertion ( 0, 0 ) <|
                        B.fromString prefix
                    ]
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


setCursor : Position -> Bool -> Buffer -> Buffer
setCursor cursor saveColumn buf =
    { buf
        | cursor = cursor
        , cursorColumn =
            if saveColumn then
                Tuple.second cursor
            else
                buf.cursorColumn
    }


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
                    |> setCursor cursor (oldModeName == V.ModeNameInsert)
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
            |> setCursor cursor True


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


isSaveColumn : V.MotionData -> Bool
isSaveColumn md =
    case md of
        V.VLineDelta _ ->
            False

        V.LineDelta _ ->
            False

        _ ->
            True


scrollToLine : Int -> Buffer -> Buffer
scrollToLine n buf =
    updateView (\v -> { v | scrollTop = n }) buf


ceilingFromZero : Float -> Int
ceilingFromZero n =
    if n < 0 then
        floor n
    else
        ceiling n


runOperator : Operator -> Buffer -> Buffer
runOperator operator buf =
    case operator of
        Move md mo ->
            case runMotion md mo buf of
                Just cursor ->
                    setCursor cursor (isSaveColumn md) buf

                Nothing ->
                    buf

        Scroll delta ->
            let
                cnt =
                    B.countLines buf.lines

                scrollTop =
                    (buf.view.scrollTop + delta)
                        |> max 0
                        |> min (cnt - 1)
            in
                buf
                    |> scrollToLine scrollTop
                    |> boundCursor

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

        JumpByView factor ->
            let
                view =
                    buf.view

                height =
                    view.size.height

                boundLine row =
                    row
                        |> max 0
                        |> min (B.countLines buf.lines - 1)

                boundScrollTop scrollTop n =
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
                    boundLine (Tuple.first buf.cursor + n)

                scrollTop =
                    boundScrollTop view.scrollTop n
            in
                case gotoLine y buf.lines of
                    Just cursor ->
                        buf
                            |> setCursor cursor True
                            |> scrollToLine scrollTop

                    Nothing ->
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
boundCursor : Buffer -> Buffer
boundCursor ({ view, cursor, lines } as buf) =
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
                    setCursor cursor True buf

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
            |> scrollToCursor
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        PressKey bufid key ->
            handleKeypress key model

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
                    |> boundCursor
                , Cmd.none
                )
