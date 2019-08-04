module Update.Yank exposing (put, yank, yankWholeBuffer)

import Dict
import Internal.Position exposing (Position)
import Internal.PositionClass exposing (findLineFirst)
import Internal.TextBuffer as B
import Internal.Window as Win
import Model exposing (Editor)
import Model.Buffer exposing (..)
import Model.BufferHistory exposing (getLastPatch)
import Model.Frame as Frame
import Model.Global exposing (..)
import Model.View as View
import Update.Buffer as Buf
import Update.Message exposing (..)
import Update.Range exposing (isLinewise, operatorRanges)
import Update.Service exposing (sendWriteClipboard)
import Vim.AST as V exposing (Operator(..))


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

        regText =
            if register == "%" then
                Win.getActiveFrame global.window
                    |> Maybe.andThen Frame.getActiveViewId
                    |> Maybe.map Text

            else if register == "#" then
                Win.getActiveFrame global.window
                    |> Maybe.andThen Frame.getAlterViewId
                    |> Maybe.map Text

            else
                Dict.get register global.registers
    in
    regText
        |> Maybe.map
            (\s ->
                case buf.mode of
                    Ex ({ exbuf } as ex) ->
                        { ed
                            | buf = setExbuf buf ex (putString forward s exbuf)
                            , global = removeRegister "+" global
                        }

                    _ ->
                        { ed
                            | buf = putString forward s buf
                            , global = removeRegister "+" global
                        }
            )
        |> Maybe.withDefault ed


putString : Bool -> RegisterText -> Buffer -> Buffer
putString forward text buf =
    let
        ( y, x ) =
            buf.view.cursor

        tabSize =
            buf.config.tabSize

        ( patch, cursor ) =
            let
                line =
                    B.getLine y buf.lines |> Maybe.withDefault ""
            in
            case text of
                Text s ->
                    if forward && line /= B.lineBreak then
                        ( s
                            |> B.fromStringExpandTabs tabSize (x + 1)
                            |> B.Insertion ( y, x + 1 )
                        , Nothing
                        )

                    else
                        ( s
                            |> B.fromStringExpandTabs tabSize x
                            |> B.Insertion ( y, x )
                        , Nothing
                        )

                Lines s ->
                    if forward then
                        let
                            s1 =
                                if String.endsWith B.lineBreak line then
                                    s

                                else
                                    B.lineBreak ++ s
                        in
                        ( s1
                            |> B.fromStringExpandTabs tabSize 0
                            |> B.Insertion ( y + 1, 0 )
                        , Just ( y + 1, findLineFirst s + 1 )
                        )

                    else
                        case buf.mode of
                            Insert _ ->
                                ( s
                                    |> B.fromStringExpandTabs tabSize x
                                    |> B.Insertion ( y, x )
                                , Nothing
                                )

                            _ ->
                                ( s
                                    |> B.fromStringExpandTabs tabSize 0
                                    |> B.Insertion ( y, 0 )
                                , Just ( y, findLineFirst s + 1 )
                                )

        getLastDeletedTo : Buffer -> Position
        getLastDeletedTo buf_ =
            case getLastPatch buf_.history of
                Just patch_ ->
                    case patch_ of
                        B.Deletion _ to ->
                            to

                        _ ->
                            buf_.view.cursor

                _ ->
                    buf_.view.cursor
    in
    { buf | dirtyIndent = 0 }
        |> Buf.transaction [ patch ]
        |> (\buf1 ->
                { buf1
                    | view =
                        View.setCursor
                            (case cursor of
                                Just p ->
                                    p

                                Nothing ->
                                    getLastDeletedTo buf1
                            )
                            True
                            buf1.view
                }
           )
