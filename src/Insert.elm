module Insert exposing (insert, openNewLine, autoIndent)

import Model exposing (..)
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Buffer as Buf
import Tuple
import PositionClass exposing (findPosition)
import String


getString : Buffer -> V.StringType -> String
getString buf ins =
    case ins of
        V.TextLiteral s ->
            s

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
            case s of
                V.TextLiteral str ->
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

                _ ->
                    buf


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
                |> Maybe.andThen
                    (\line ->
                        findPosition ""
                            V.LineFirst
                            (V.motionOption "<]$=")
                            line
                            0
                    )
                |> Maybe.withDefault 0
    in
        String.repeat x " "


openNewLine : Int -> Buffer -> Buffer
openNewLine y buf =
    let
        n =
            B.countLines buf.lines

        y1 =
            y
                |> max 0
                |> min n

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
