module Update.Indent exposing (applyIndent)

import Internal.PositionClass exposing (findLineFirst)
import Internal.TextBuffer as B exposing (Patch(..))
import Model exposing (..)
import Model.Buffer exposing (..)
import Model.Global exposing (..)
import Update.Buffer as Buf
import Update.Range exposing (operatorRanges)
import Vim.AST as V exposing (ChangeCase(..), Operator(..))


applyIndent : Maybe Int -> Bool -> V.OperatorRange -> Global -> Buffer -> Buffer
applyIndent count forward range global buf =
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
                |> operatorRanges count range global
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
                                |> (+)
                                    (if x == 0 then
                                        -1

                                     else
                                        0
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
