module Update.Yank exposing (yank, put)

import Model exposing (RegisterText(..), Buffer, Mode(..))
import Vim.AST as V exposing (Operator(..))
import Update.Range exposing (operatorRanges, isLinewise)
import Update.Buffer as Buf
import Internal.TextBuffer as B
import Update.Message exposing (..)
import Update.Service exposing (sendWriteClipboard)
import Dict


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
            |> Buf.setRegister "0" txt
            |> Buf.setRegister
                (if register == "+" then
                    "\""
                 else
                    register
                )
                txt
        , cmd
        )


put : String -> Bool -> Buffer -> Buffer
put register forward buf =
    let
        removeRegister reg buf_ =
            { buf_ | registers = Dict.remove reg buf_.registers }
    in
        Dict.get register buf.registers
            |> Maybe.map
                (\s ->
                    case buf.mode of
                        Ex ({ exbuf } as ex) ->
                            buf
                                |> Buf.setMode
                                    (Ex
                                        { ex
                                            | exbuf =
                                                Buf.putString
                                                    forward
                                                    s
                                                    exbuf
                                        }
                                    )
                                |> removeRegister "+"

                        _ ->
                            buf
                                |> Buf.putString forward s
                                |> removeRegister "+"
                )
            |> Maybe.withDefault buf
