module Update.CaseOperator exposing (applyCaseOperator)

import Char exposing (toLower, toUpper)
import Helper.Helper exposing (getLast, swapCase)
import Internal.TextBuffer as B exposing (Patch(..))
import Model exposing (..)
import Model.Buffer exposing (..)
import Model.Global exposing (..)
import Model.Lint exposing (..)
import Update.Buffer as Buf
import Update.Range exposing (operatorRanges)
import Vim.AST as V exposing (ChangeCase(..), Operator(..))


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
                            |> Buf.updateView (Buf.setCursor buf_.view.cursor True)
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
        |> Buf.updateView (setCursor regions)
