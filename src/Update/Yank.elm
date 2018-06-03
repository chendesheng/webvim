module Update.Yank exposing (..)

import Model exposing (RegisterText(..), Buffer, Mode(..))
import Vim.AST as V exposing (Operator(..))
import Update.Range exposing (operatorRanges, isLinewise)
import Update.Buffer exposing (setRegister)
import Internal.TextBuffer as B
import String
import Update.Message exposing (..)
import Update.Service exposing (sendWriteClipboard)


yank : Maybe Int -> String -> V.OperatorRange -> Buffer -> ( Buffer, Cmd Msg )
yank count register range buf =
    let
        s =
            operatorRanges count range buf
                --|> Debug.log "operatorRanges"
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

        --|> Debug.log "yank"
        txt =
            if isLinewise range buf.mode then
                Lines s
            else
                Text s

        cmd =
            if register == "+" then
                sendWriteClipboard buf.config.service s
            else
                Cmd.none
    in
        ( buf
            |> setRegister "0" txt
            |> setRegister
                (if register == "+" then
                    "\""
                 else
                    register
                )
                txt
        , cmd
        )
