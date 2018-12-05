module Update.CaseOperator exposing (applyCaseOperator)

import Vim.AST as V exposing (Operator(..), ChangeCase(..))
import Model exposing (..)
import Update.Range exposing (operatorRanges)
import Helper.Helper exposing (swapCase, getLast)
import Char exposing (toUpper, toLower)
import Update.Buffer as Buf
import Internal.TextBuffer as B exposing (Patch(..))


applyCaseOperator :
    Maybe Int
    -> V.ChangeCase
    -> V.OperatorRange
    -> Global
    -> Buffer
    -> Buffer
applyCaseOperator count changeCase range global buf =
    let
        regions =
            operatorRanges count range global buf

        setCursor regions_ buf_ =
            case getLast regions_ of
                Just ( b, e ) ->
                    Buf.setCursor b True buf_

                _ ->
                    buf_
    in
        regions
            |> List.foldl
                (\( b, e ) buf1 ->
                    let
                        s =
                            buf1.lines
                                |> B.sliceRegion b e
                                |> B.toString

                        replaceChar map buf_ =
                            buf_
                                |> Buf.transaction
                                    [ Deletion b e
                                    , s
                                        |> String.map map
                                        |> B.fromString
                                        |> Insertion b
                                    ]
                                |> Buf.setCursor buf_.view.cursor True
                    in
                        case changeCase of
                            LowerCase ->
                                replaceChar toLower buf1

                            UpperCase ->
                                replaceChar toUpper buf1

                            SwapCase ->
                                replaceChar swapCase buf1
                )
                buf
            |> setCursor regions
