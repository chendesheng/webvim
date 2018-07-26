module Update.CaseOperator exposing (applyCaseOperator)

import Vim.AST as V exposing (Operator(..), ChangeCase(..))
import Model exposing (..)
import Update.Range exposing (operatorRanges, shrinkRight)
import Helper.Helper exposing (swapCase)
import Char exposing (toUpper, toLower)
import Update.Buffer as Buf
import Internal.TextBuffer as B exposing (Patch(..))


applyCaseOperator :
    Maybe Int
    -> V.ChangeCase
    -> V.OperatorRange
    -> Buffer
    -> Buffer
applyCaseOperator count changeCase range buf =
    let
        region =
            operatorRanges count range buf
                |> List.head
    in
        case region of
            Just ( begin, end ) ->
                let
                    s =
                        buf.lines
                            |> B.sliceRegion begin end
                            |> B.toString

                    replaceChar map buf =
                        buf
                            |> Buf.transaction
                                [ Deletion begin end
                                , s
                                    |> String.map map
                                    |> B.fromString
                                    |> Insertion begin
                                ]
                            |> Buf.setCursor buf.cursor True
                in
                    case changeCase of
                        LowerCase ->
                            replaceChar toLower buf

                        UpperCase ->
                            replaceChar toUpper buf

                        SwapCase ->
                            replaceChar swapCase buf

            _ ->
                buf
