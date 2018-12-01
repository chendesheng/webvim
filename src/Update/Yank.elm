module Update.Yank exposing (yank, put, yankWholeBuffer)

import Model exposing (RegisterText(..), Buffer, Mode(..), Editor, Global)
import Vim.AST as V exposing (Operator(..))
import Update.Range exposing (operatorRanges, isLinewise)
import Update.Buffer as Buf
import Internal.TextBuffer as B
import Update.Message exposing (..)
import Update.Service exposing (sendWriteClipboard)
import Dict


yankWholeBuffer : Editor -> ( Editor, Cmd Msg )
yankWholeBuffer ({ buf, global } as ed) =
    ( { ed | buf = Buf.infoMessage "Whole buffer copied." buf }
    , buf.lines
        |> B.toString
        |> sendWriteClipboard global.service
    )


yank : Maybe Int -> String -> V.OperatorRange -> Editor -> ( Editor, Cmd Msg )
yank count register range ({ global, buf } as ed) =
    let
        s =
            operatorRanges count range global buf
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
                sendWriteClipboard global.service s
            else
                Cmd.none
    in
        ( { ed
            | global =
                global
                    |> Buf.setRegister "0" txt
                    |> Buf.setRegister
                        (if register == "+" then
                            "\""
                         else
                            register
                        )
                        txt
          }
        , cmd
        )


put : String -> Bool -> Editor -> Editor
put register forward ({ global, buf } as ed) =
    let
        removeRegister reg global_ =
            { global
                | registers = Dict.remove reg global_.registers
            }
    in
        Dict.get register global.registers
            |> Maybe.map
                (\s ->
                    case buf.mode of
                        Ex ({ exbuf } as ex) ->
                            { ed
                                | buf =
                                    Buf.setMode
                                        (Ex
                                            { ex
                                                | exbuf =
                                                    Buf.putString forward
                                                        s
                                                        exbuf
                                            }
                                        )
                                        buf
                                , global = removeRegister "+" global
                            }

                        _ ->
                            { ed
                                | buf = Buf.putString forward s buf
                                , global = removeRegister "+" global
                            }
                )
            |> Maybe.withDefault ed
