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
import Dict


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


saveLastDeleted : String -> Buffer -> Buffer
saveLastDeleted reg buf =
    let
        s =
            buf
                |> Buf.getLastDeleted
                |> Maybe.map B.toString
                |> Maybe.withDefault ""
    in
        Buf.setRegister reg s buf


type alias Transaction =
    { pos : Maybe ( Position, Bool )
    , patches : List Patch
    }


applyTransaction : Transaction -> Buffer -> Buffer
applyTransaction { pos, patches } buf =
    case pos of
        Just ( cursor, saveColumn ) ->
            buf
                |> Buf.setCursor cursor saveColumn
                |> Buf.transaction patches

        _ ->
            Buf.transaction patches buf


deleteOperator : V.OperatorRange -> Buffer -> Maybe Transaction
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
                            Just
                                { pos = Just ( pos, False )
                                , patches =
                                    [ Deletion ( Tuple.first begin, 0 )
                                        ( if mo.inclusive then
                                            endy + 1
                                          else
                                            endy
                                        , 0
                                        )
                                    ]
                                }
                        else
                            Just
                                { pos = Nothing
                                , patches =
                                    [ Deletion begin
                                        ( endy
                                        , if mo.inclusive then
                                            endx + 1
                                          else
                                            endx
                                        )
                                    ]
                                }

                Nothing ->
                    Nothing

        _ ->
            Nothing


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

                V.RepeatMatchChar ->
                    case buf.last.matchChar of
                        Just { char, before, forward } ->
                            let
                                option =
                                    V.motionOption "<]$-"

                                option1 =
                                    { option
                                        | forward =
                                            if forward then
                                                mo.forward
                                            else
                                                not mo.forward
                                    }
                            in
                                findPositionInBuffer
                                    (V.MatchChar char before)
                                    option1
                                    y
                                    x
                                    ""
                                    buf.config.wordChars
                                    buf.lines

                        _ ->
                            Nothing

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
                    |> Buf.setCursor cursor (oldModeName == V.ModeNameInsert)
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
            |> Buf.setCursor cursor True


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


saveMotion : Operator -> Buffer -> Buffer
saveMotion operator buf =
    let
        last =
            buf.last

        last1 =
            case operator of
                Move md mo ->
                    case md of
                        V.MatchChar ch before ->
                            { last
                                | matchChar =
                                    Just
                                        { char = ch
                                        , before = before
                                        , forward = mo.forward
                                        }
                            }

                        _ ->
                            buf.last

                _ ->
                    buf.last
    in
        { buf | last = last1 }


runOperator : String -> Operator -> Buffer -> Buffer
runOperator register operator buf =
    case operator of
        Move md mo ->
            case runMotion md mo buf of
                Just cursor ->
                    buf
                        |> Buf.setCursor cursor (isSaveColumn md)
                        |> saveMotion operator

                Nothing ->
                    buf
                        |> saveMotion operator

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
            case buf.mode of
                Ex prefix exbuf ->
                    Buf.setMode
                        (exbuf
                            |> insertString s
                            |> Ex prefix
                        )
                        buf

                _ ->
                    insertString s buf

        Delete rg ->
            case buf.mode of
                Ex prefix exbuf ->
                    Buf.setMode
                        ((case deleteOperator rg exbuf of
                            Just trans ->
                                applyTransaction trans exbuf

                            _ ->
                                exbuf
                         )
                            |> Ex prefix
                        )
                        buf

                Insert ->
                    case deleteOperator rg buf of
                        Just trans ->
                            applyTransaction trans buf

                        _ ->
                            buf

                _ ->
                    case deleteOperator rg buf of
                        Just trans ->
                            buf
                                |> applyTransaction trans
                                |> saveLastDeleted register

                        _ ->
                            buf

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
                    Buf.setCursor cursor True buf

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


handleKeypress : Key -> Buffer -> ( Buffer, Cmd Msg )
handleKeypress key buf =
    let
        ( { edit, modeName, register } as ast, continuation ) =
            P.parse buf.continuation key

        oldModeName =
            -- for now the put operator is implemented as
            -- Start insert mode -> Put string -> Back to normal mode
            if modeName == V.ModeNameNormal && isPutOperator edit then
                V.ModeNameInsert
            else
                getModeName buf.mode
    in
        ( buf
            |> setContinuation continuation
            |> applyEdit edit register
            |> updateMode modeName
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
                    |> cursorScope
                , Cmd.none
                )
