module Update.Insert exposing (insert, openNewLine, autoIndent)

import Model exposing (..)
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Update.Buffer as Buf
import Tuple
import Internal.PositionClass exposing (findPosition, findLineFirst)
import String
import Update.Motion exposing (wordStringUnderCursor)


getString : Buffer -> V.StringType -> String
getString buf ins =
    case ins of
        V.TextLiteral s ->
            s

        V.WordUnderCursor ->
            wordStringUnderCursor
                buf.config.wordChars
                buf.lines
                buf.cursor
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
            in
                if str == B.lineBreak then
                    let
                        indent =
                            autoIndent (Tuple.first buf.cursor) buf.lines
                    in
                        buf
                            |> setLastIndent (String.length indent)
                            |> insertString (str ++ indent)
                else
                    buf
                        |> setLastIndent 0
                        |> insertString str


setLastIndent : Int -> Buffer -> Buffer
setLastIndent indent buf =
    let
        last =
            buf.last
    in
        { buf | last = { last | indent = indent } }


autoIndent : Int -> B.TextBuffer -> String
autoIndent y lines =
    let
        x =
            lines
                |> B.getLine y
                |> Maybe.map findLineFirst
                |> Maybe.withDefault 0
    in
        String.repeat x " "


openNewLine : Int -> Buffer -> Buffer
openNewLine y buf =
    let
        y1 =
            y
                |> max 0
                |> min (B.count buf.lines)

        indent =
            autoIndent (y1 - 1) buf.lines

        x =
            String.length indent

        patch =
            Insertion ( y1, 0 ) <| B.fromString (indent ++ B.lineBreak)

        last =
            buf.last
    in
        buf
            |> setLastIndent x
            |> Buf.transaction [ patch ]
            |> Buf.setCursor ( y1, x ) True
