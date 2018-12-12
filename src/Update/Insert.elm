module Update.Insert exposing (insert, openNewLine)

import Helper.Helper exposing (regex)
import Internal.PositionClass exposing (findLineFirst, findPosition)
import Internal.TextBuffer as B exposing (Patch(..))
import Model exposing (..)
import Regex as Re
import String
import Tuple
import Update.Buffer as Buf
import Update.Motion exposing (wordStringUnderCursor)
import Vim.AST as V exposing (Operator(..))


getString : Buffer -> V.StringType -> String
getString buf ins =
    case ins of
        V.TextLiteral s ->
            s

        V.WordUnderCursor ->
            wordStringUnderCursor
                buf.config.wordChars
                buf.lines
                buf.view.cursor
                |> Maybe.map Tuple.second
                |> Maybe.withDefault ""

        _ ->
            ""


insertString : String -> Buffer -> Buffer
insertString s buf =
    let
        { expandTab, tabSize } =
            buf.config

        s1 =
            if expandTab then
                B.fromStringExpandTabs
                    tabSize
                    (buf.view.cursor
                        |> Tuple.second
                    )
                    s

            else
                B.fromString s
    in
    Buf.transaction
        [ Insertion buf.view.cursor s1 ]
        buf


insert : V.StringType -> Buffer -> Buffer
insert s buf =
    case buf.mode of
        Ex ({ exbuf } as ex) ->
            Buf.setMode
                (Ex { ex | exbuf = insertString (getString buf s) exbuf })
                buf

        _ ->
            let
                str =
                    getString buf s

                lastIndent =
                    buf.dirtyIndent
            in
            if str == B.lineBreak then
                if buf.dirtyIndent > 0 then
                    let
                        keepLastIndent indent buf_ =
                            buf_
                                |> Buf.transaction
                                    [ indent
                                        |> repeatSpace
                                        |> B.fromString
                                        |> Insertion
                                            ( Tuple.first buf_.view.cursor, 0 )
                                    ]
                                |> Buf.setLastIndent indent
                    in
                    buf
                        |> Buf.cancelLastIndent
                        |> insertString str
                        |> keepLastIndent lastIndent

                else
                    buf
                        |> insertString str
                        |> autoIndent

            else if
                case buf.config.indent of
                    IndentRules { trigger } ->
                        String.contains str trigger

                    _ ->
                        False
            then
                buf
                    |> Buf.setLastIndent 0
                    |> insertString str
                    |> autoIndent

            else
                buf
                    |> Buf.setLastIndent 0
                    |> insertString str


repeatSpace : Int -> String
repeatSpace n =
    String.repeat n " "


{-| indent after insert
-}
autoIndent : Buffer -> Buffer
autoIndent buf =
    let
        ( y, _ ) =
            buf.view.cursor

        indent =
            calcIndent buf.config.indent buf.config.tabSize y buf.lines

        insertIndent =
            indent
                |> repeatSpace
                |> B.fromString
                |> Insertion ( y, 0 )

        deleteIndent =
            buf.lines
                |> B.getLine y
                |> Maybe.map
                    (\line ->
                        let
                            indent_ =
                                findLineFirst line
                        in
                        if indent_ > 0 then
                            [ Deletion ( y, 0 ) ( y, indent_ ) ]

                        else
                            []
                    )
                |> Maybe.withDefault []

        saveLastIndent : Buffer -> Buffer
        saveLastIndent buf_ =
            let
                ( y_, _ ) =
                    buf_.view.cursor

                isBlank =
                    Re.contains (regex "\\S")
                        >> not
            in
            if
                buf_.lines
                    |> B.getLine y_
                    |> Maybe.map isBlank
                    |> Maybe.withDefault False
            then
                Buf.setLastIndent indent buf_

            else
                buf_
    in
    buf
        |> Buf.transaction (deleteIndent ++ [ insertIndent ])
        |> Buf.updateView (Buf.setCursorColumn indent)
        |> saveLastIndent


calcIndent : IndentConfig -> Int -> Int -> B.TextBuffer -> Int
calcIndent config tabSize y lines =
    let
        baseIndent =
            lines
                |> B.getLine (y - 1)
                |> Maybe.map findLineFirst
                |> Maybe.withDefault 0
    in
    case config of
        AutoIndent ->
            baseIndent

        IndentRules rules ->
            let
                { increase, decrease, increaseNext } =
                    rules

                getLine i =
                    lines
                        |> B.getLine i
                        |> Maybe.map String.trim

                prevTwoLine =
                    getLine (y - 2)
                        |> Maybe.map
                            (\line ->
                                if Re.contains increaseNext line then
                                    -1

                                else
                                    0
                            )
                        |> Maybe.withDefault 0

                prevLine =
                    getLine (y - 1)
                        |> Maybe.map
                            (\line ->
                                if
                                    Re.contains increase line
                                        || Re.contains increaseNext line
                                then
                                    1

                                else
                                    0
                            )
                        |> Maybe.withDefault 0

                currentLine =
                    getLine y
                        |> Maybe.map
                            (\line ->
                                if Re.contains decrease line then
                                    -1

                                else
                                    0
                            )
                        |> Maybe.withDefault 0
            in
            baseIndent
                + (prevTwoLine + prevLine + currentLine)
                * tabSize


openNewLine : Int -> Buffer -> Buffer
openNewLine y buf =
    let
        y1 =
            y
                |> max 0
                |> min (B.count buf.lines)
    in
    buf
        |> Buf.transaction
            [ B.lineBreak
                |> B.fromString
                |> Insertion ( y1, 0 )
            ]
        |> Buf.updateView (Buf.setCursor ( y1, 0 ) False)
        |> autoIndent
