module Insert exposing (insert, openNewLine)

import Model exposing (..)
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Buffer as Buf
import Regex as Re
import Tuple
import Motion exposing (matchString)


getString : Buffer -> V.StringType -> String
getString buf ins =
    case ins of
        V.TextLiteral s ->
            s

        _ ->
            ""


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


insert : V.StringType -> Buffer -> Buffer
insert s buf =
    case buf.mode of
        Ex prefix exb ->
            let
                exbuf =
                    insertString s exb

                prefix1 =
                    case prefix of
                        ExSearch forward _ ->
                            let
                                re =
                                    exbuf.lines
                                        |> B.toString
                                        |> String.dropLeft 1
                                        |> Re.regex
                            in
                                buf.lines
                                    |> matchString forward re buf.cursor
                                    |> ExSearch forward

                        _ ->
                            prefix
            in
                exbuf
                    |> Ex prefix1
                    |> flip Buf.setMode buf

        _ ->
            insertString s buf


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
