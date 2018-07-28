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
    -> Buffer
    -> Buffer
applyCaseOperator count changeCase range buf =
    let
        regions =
            operatorRanges count range buf

        setCursor regions buf =
            case getLast regions of
                Just ( b, e ) ->
                    Buf.setCursor b True buf

                _ ->
                    buf
    in
        regions
            |> List.foldl
                (\( b, e ) buf ->
                    let
                        s =
                            buf.lines
                                |> B.sliceRegion b e
                                |> B.toString

                        replaceChar map buf =
                            buf
                                |> Buf.transaction
                                    [ Deletion b e
                                    , s
                                        |> String.map map
                                        |> B.fromString
                                        |> Insertion b
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
                )
                buf
            |> setCursor regions
