module Update.Indent exposing (applyIndent)

import Vim.AST as V exposing (Operator(..), ChangeCase(..))
import Model exposing (..)
import Update.Range exposing (operatorRanges)
import Update.Buffer as Buf
import Internal.TextBuffer as B exposing (Patch(..))
import Internal.PositionClass exposing (findLineFirst)


applyIndent : Maybe Int -> Bool -> V.OperatorRange -> Buffer -> Buffer
applyIndent count forward range buf =
    let
        genPatches y =
            if forward then
                buf.lines
                    |> B.getLine y
                    |> Maybe.map
                        (\s ->
                            " "
                                |> String.repeat
                                    (if String.length s > 1 then
                                        buf.config.tabSize
                                     else
                                        0
                                    )
                                |> B.fromString
                                |> Insertion ( y, 0 )
                        )
                    |> Maybe.withDefault (Insertion ( y, 0 ) B.empty)
            else
                let
                    size =
                        buf.lines
                            |> B.getLine y
                            |> Maybe.map findLineFirst
                            |> Maybe.withDefault 0
                            |> min
                                buf.config.tabSize
                in
                    Deletion
                        ( y, 0 )
                        ( y, size )

        lineNumbers =
            buf
                |> operatorRanges count range
                |> List.concatMap
                    (\rg ->
                        let
                            ( begin, end ) =
                                rg

                            ( y, x ) =
                                end
                        in
                            List.range
                                (Tuple.first begin)
                                (y
                                    |> ((+)
                                            (if x == 0 then
                                                -1
                                             else
                                                0
                                            )
                                       )
                                    |> max 0
                                )
                    )
    in
        case lineNumbers of
            y :: _ ->
                let
                    buf1 =
                        Buf.transaction
                            (List.map genPatches lineNumbers)
                            buf
                in
                    Buf.gotoLine y buf1

            _ ->
                buf
