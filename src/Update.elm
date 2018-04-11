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
import TextObject exposing (expandTextObject)
import Result
import List
import Tuple
import String
import Service exposing (..)


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


modeChanged : Bool -> Key -> V.ModeName -> Buffer -> Buffer
modeChanged replaying key oldModeName buf =
    case buf.mode of
        Normal ->
            let
                ( y, x ) =
                    buf.cursor

                lastIndent =
                    if oldModeName == V.ModeNameInsert then
                        case buf.last.indent of
                            x ->
                                if B.getLineMaxColumn y buf.lines == x then
                                    x
                                else
                                    0
                    else
                        0

                cursor =
                    if oldModeName == V.ModeNameInsert then
                        if lastIndent > 0 then
                            ( y, 0 )
                        else
                            ( y, max (x - 1) 0 )
                    else
                        ( y
                        , if B.getLineMaxColumn y buf.lines > x then
                            x
                          else
                            max (x - 1) 0
                        )

                buf1 =
                    if lastIndent > 0 then
                        Buf.transaction
                            [ Deletion ( y, 0 ) ( y, lastIndent ) ]
                            buf
                    else
                        buf
            in
                buf1
                    |> Buf.setCursor
                        cursor
                        (cursor /= buf.cursor)
                    |> Buf.commit

        TempNormal ->
            buf
                |> Buf.commit

        Ex ({ prefix, exbuf } as ex) ->
            if B.isEmpty exbuf.lines then
                buf
                    |> handleKeypress False "<esc>"
                    |> Tuple.first
            else
                let
                    last =
                        buf.last

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
                                            |> Re.caseInsensitive
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

                    buf1 =
                        if replaying || key == "<exbuf>" then
                            buf
                        else
                            case oldModeName of
                                V.ModeNameEx _ ->
                                    { buf
                                        | last = { last | ex = last.ex ++ key }
                                    }

                                _ ->
                                    { buf | last = { last | ex = "" } }
                in
                    Buf.setMode
                        (Ex { ex | prefix = prefix1 })
                        buf1

        Insert ->
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

        Visual _ ->
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


expandVisual :
    comparable
    -> comparable
    -> comparable
    -> comparable
    -> ( comparable, comparable )
expandVisual begin end a b =
    if begin < end then
        ( (min a begin), (max b end) )
    else
        ( (max b begin), (min a end) )


runOperator : String -> Operator -> Buffer -> Buffer
runOperator register operator buf =
    case operator of
        Move md mo ->
            motion md mo buf

        Select textobj around ->
            case buf.mode of
                Visual { tipe, begin, end } ->
                    case tipe of
                        V.VisualChars ->
                            (expandTextObject buf.config.wordChars
                                textobj
                                around
                                end
                                buf.lines
                            )
                                |> Maybe.map
                                    (\rg ->
                                        let
                                            ( a, b ) =
                                                rg

                                            ( begin1, end1 ) =
                                                expandVisual begin end a b
                                        in
                                            buf
                                                |> Buf.setCursor end1 True
                                                |> setVisualEnd end1
                                                |> setVisualBegin begin1
                                    )
                                |> Maybe.withDefault buf

                        _ ->
                            buf

                _ ->
                    buf

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

        OpenNewLine forward ->
            openNewLine
                (if forward then
                    Tuple.first buf.cursor + 1
                 else
                    Tuple.first buf.cursor
                )
                buf

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
                                                Buf.putString
                                                    forward
                                                    s
                                                    exbuf
                                        }
                                    )
                                    buf

                            _ ->
                                Buf.putString forward s buf
                    )
                |> Maybe.withDefault buf

        RepeatLastOperator ->
            replayKeys buf.dotRegister buf

        RepeatLastInsert ->
            replayKeys buf.last.inserts buf

        RepeatLastVisual ->
            replayKeys buf.last.visual buf

        RepeatLastEx ->
            replayKeys buf.last.ex buf

        VisualSwitchEnd ->
            case buf.mode of
                Visual { tipe, begin, end } ->
                    buf
                        |> Buf.setMode
                            (Visual
                                { tipe = tipe
                                , begin = end
                                , end = begin
                                }
                            )
                        |> Buf.setCursor begin True

                _ ->
                    buf

        Execute ->
            buf

        Join mergeSpaces ->
            join mergeSpaces buf

        Replace ch ->
            case buf.mode of
                Normal ->
                    let
                        ( y, x ) =
                            buf.cursor

                        setCursor =
                            if ch == B.lineBreak then
                                identity
                            else
                                Buf.setCursor buf.cursor True
                    in
                        buf
                            |> Buf.transaction
                                [ Deletion buf.cursor ( y, x + 1 ) ]
                            |> insert (V.TextLiteral ch)
                            |> setCursor

                --|> Buf.setCursor buf.cursor True
                TempNormal ->
                    buf

                Visual visual ->
                    buf

                _ ->
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


isEnterInsertMode : Maybe Operator -> Bool
isEnterInsertMode edit =
    case edit of
        Just operator ->
            case operator of
                Put _ ->
                    True

                Replace _ ->
                    True

                _ ->
                    False

        _ ->
            False


replayKeys : String -> Model -> Model
replayKeys s buf =
    if s == "" then
        buf
    else
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


execute : String -> Buffer -> Cmd Msg
execute s buf =
    case String.split " " s of
        [ "e", path ] ->
            sendEditBuffer buf.service path

        [ "w" ] ->
            sendSaveBuffer buf.service buf.path buf

        [ "w", path ] ->
            sendSaveBuffer buf.service path buf

        _ ->
            Cmd.none


getEffect : Maybe Operator -> Buffer -> Cmd Msg
getEffect op buf =
    case op of
        Just op1 ->
            case op1 of
                Execute ->
                    case buf.mode of
                        Ex { exbuf } ->
                            exbuf.lines
                                |> B.toString
                                |> String.dropLeft 1
                                |> flip execute buf

                        _ ->
                            Cmd.none

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


handleKeypress : Bool -> Key -> Buffer -> ( Buffer, Cmd Msg )
handleKeypress replaying key buf =
    let
        cacheKey =
            ( buf.continuation, key )

        (( ast, continuation ) as cacheVal) =
            case Dict.get cacheKey buf.vimASTCache of
                Just resp ->
                    resp

                _ ->
                    parse buf.continuation key

        { edit, modeName, register, recordKeys } =
            ast

        -- |> Debug.log key
        oldModeName =
            -- For now the put operator is implemented as
            --   1) Start insert mode
            --   2) Put string
            --   3) Back to normal mode
            if modeName == V.ModeNameNormal && isEnterInsertMode edit then
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
                        { buf | dotRegister = s }
    in
        ( buf
            |> cacheVimAST cacheKey cacheVal
            |> setContinuation continuation
            |> applyEdit edit register
            |> updateMode modeName
            |> modeChanged replaying key oldModeName
            |> scrollToCursor
            |> saveDotRegister
        , getEffect edit buf
        )


filename : String -> ( String, String )
filename s =
    case
        Re.find
            (Re.AtMost 1)
            (Re.regex "(^|[/\\\\])([^.]+)([.][^.]*)?$")
            s
    of
        [ m ] ->
            case m.submatches of
                [ _, a, b ] ->
                    ( Maybe.withDefault "" a
                    , Maybe.withDefault "" b
                    )

                _ ->
                    ( "", "" )

        _ ->
            ( "", "" )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        PressKey bufid key ->
            handleKeypress False key model

        Resize size ->
            let
                h =
                    (size.height // model.view.lineHeight)
                        - model.view.statusbarHeight

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

        Read result ->
            case result of
                Ok { path, content } ->
                    let
                        lines =
                            B.fromString content

                        ( name, ext ) =
                            filename path

                        syntax =
                            { lang =
                                if ext == "" then
                                    ""
                                else
                                    String.dropLeft 1 ext
                            , lines = emptyBuffer.syntax.lines
                            }
                    in
                        ( { emptyBuffer
                            | lines = lines
                            , view =
                                { emptyView
                                    | size = model.view.size
                                    , lineHeight = model.view.lineHeight
                                }
                            , path = path
                            , name = name ++ ext
                            , syntax =
                                Buf.syntaxHighlight
                                    0
                                    lines
                                    syntax
                            , service = model.service
                          }
                        , Cmd.none
                        )

                Err s ->
                    ( model, Cmd.none )

        Write _ ->
            let
                history =
                    model.history
            in
                ( { model
                    | history = { history | savePoint = history.version }
                  }
                , Cmd.none
                )
