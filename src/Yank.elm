module Yank exposing (..)

import Model exposing (RegisterText(..), Buffer, Mode(..))
import Vim.AST as V exposing (Operator(..))
import Range exposing (operatorRanges, isLinewise)
import Buffer exposing (setRegister)
import Internal.TextBuffer as B
import String


yank : String -> V.OperatorRange -> Buffer -> Buffer
yank register range buf =
    let
        s =
            operatorRanges range buf
                |> Debug.log "operatorRanges"
                |> List.map
                    (\rg ->
                        let
                            ( a, b ) =
                                rg
                        in
                            buf.lines
                                |> B.substring a b
                                |> B.toString
                    )
                |> String.join ""
                |> Debug.log "yank"

        txt =
            if isLinewise range buf.mode then
                Lines s
            else
                Text s
    in
        buf
            |> setRegister "0" txt
            |> setRegister register txt
