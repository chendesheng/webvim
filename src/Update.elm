module Update exposing (update)

import Model exposing (..)
import Message exposing (..)


-- import Vim.Parser as P

import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B


runMotion : V.Motion -> Buffer -> Buffer
runMotion motion buf =
    if B.isEmpty buf.lines then
        buf
    else
        case motion of
            _ ->
                buf


runInsertString : String -> Buffer -> Buffer
runInsertString s buf =
    buf


updateMode : V.Mode -> Buffer -> Buffer
updateMode mode buf =
    let
        newModeName =
            mode.mode

        newMode =
            case buf.mode of
                Normal ->
                    case newModeName of
                        V.ModeNameInsert ->
                            Insert

                        V.ModeNameEx prefix ->
                            Ex
                                { prefix = prefix
                                , buffer = ""
                                , cursor = ( 0, 0 )
                                }

                        V.ModeNameVisual _ ->
                            Visual VisualRange []

                        _ ->
                            Normal

                Visual _ _ ->
                    case newModeName of
                        V.ModeNameInsert ->
                            Insert

                        V.ModeNameEx prefix ->
                            Ex
                                { prefix = prefix
                                , buffer = ""
                                , cursor = ( 0, 0 )
                                }

                        V.ModeNameVisual _ ->
                            buf.mode

                        _ ->
                            Normal

                Ex _ ->
                    case newModeName of
                        V.ModeNameInsert ->
                            Insert

                        V.ModeNameVisual _ ->
                            Visual VisualRange []

                        V.ModeNameEx _ ->
                            buf.mode

                        _ ->
                            Normal

                Insert ->
                    case newModeName of
                        V.ModeNameEx prefix ->
                            Ex
                                { prefix = prefix
                                , buffer = ""
                                , cursor = ( 0, 0 )
                                }

                        V.ModeNameVisual _ ->
                            Visual VisualRange []

                        V.ModeNameInsert ->
                            buf.mode

                        _ ->
                            Normal
    in
        { buf | mode = newMode }


applyChange : V.Mode -> Model -> Model
applyChange change model =
    let
        buf =
            updateMode change model.activeBuffer
    in
        Maybe.map
            (\op ->
                let
                    newBuf =
                        case op of
                            Move motion ->
                                runMotion motion buf

                            InsertString s ->
                                runInsertString s buf

                            _ ->
                                buf
                in
                    updateActiveBuffer (always newBuf) model
            )
            change.edit
            |> Maybe.withDefault model


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    ( model, Cmd.none )



--    case message of
--        PressKey key ->
--            let
--                ( mode, continuation ) =
--                    P.parse model.activeBuffer.continuation key
--
--                newModel =
--                    applyChange mode model
--            in
--                ( updateActiveBuffer
--                    (\buf ->
--                        { buf | continuation = continuation }
--                    )
--                    newModel
--                , Cmd.none
--                )
