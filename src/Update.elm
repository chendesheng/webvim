module Update exposing (update)

import Model exposing (..)
import Message exposing (..)
import Vim.Parser as P
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B
import Buffer as Buf
import Types exposing (Patch(..), Position)
import Dict


moveByClass : V.PositionClass -> V.Direction -> Buffer -> Position
moveByClass class direction buf =
    let
        ( y, x ) =
            buf.cursor
    in
        case class of
            V.CharStart ->
                let
                    lineMax =
                        B.getLine y buf.lines
                            |> Maybe.map
                                (\s ->
                                    let
                                        len =
                                            String.length s
                                    in
                                        if String.endsWith B.lineBreak s then
                                            len - 2
                                        else
                                            len - 1
                                )
                            |> Maybe.withDefault 0

                    newx =
                        if direction == V.Forward then
                            x + 1
                        else
                            x - 1
                in
                    ( y, min (max newx 0) lineMax )

            _ ->
                buf.cursor


runMotion : V.Motion -> Buffer -> Position
runMotion motion buf =
    if B.isEmpty buf.lines then
        ( 0, 0 )
    else
        case motion of
            V.ByClass { class, direction } ->
                moveByClass class direction buf

            _ ->
                buf.cursor


emptyMode : V.ModeName -> Mode
emptyMode modeName =
    case modeName of
        V.ModeNameNormal ->
            Normal

        V.ModeNameTempNormal ->
            TempNormal

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


getModeName : Mode -> V.ModeName
getModeName mode =
    case mode of
        Normal ->
            V.ModeNameNormal

        Insert ->
            V.ModeNameInsert

        TempNormal ->
            V.ModeNameTempNormal

        Visual tipe _ ->
            V.ModeNameVisual
                (case tipe of
                    VisualRange ->
                        V.VisualName

                    VisualLine ->
                        V.VisualNameLine

                    VisualBlock ->
                        V.VisualNameBlock
                )

        Ex _ ->
            V.ModeNameEx ""


setCursor : Position -> Buffer -> Buffer
setCursor cursor buf =
    { buf | cursor = cursor }


updateMode : V.ModeName -> Buffer -> Buffer
updateMode modeName buf =
    let
        newModeName =
            getModeName buf.mode

        newMode =
            if newModeName == modeName then
                buf.mode
            else
                emptyMode newModeName

        buf1 =
            if
                modeName
                    == V.ModeNameInsert
                    && newModeName
                    == V.ModeNameNormal
            then
                let
                    ( y, x ) =
                        buf.cursor
                in
                    buf
                        |> setCursor ( y, max (x - 1) 0 )
                        |> Buf.commit
            else
                buf
    in
        { buf1 | mode = newMode }


modeChanged : V.ModeName -> V.ModeName -> Buffer -> Buffer
modeChanged oldModeName newModeName buf =
    if
        oldModeName
            == V.ModeNameInsert
            && newModeName
            == V.ModeNameNormal
    then
        let
            ( y, x ) =
                buf.cursor
        in
            buf
                |> setCursor ( y, max (x - 1) 0 )
                |> Buf.commit
    else
        buf


handleKeypress : Key -> Model -> Buffer -> ( Model, Cmd Msg )
handleKeypress key model buf =
    let
        oldModeName =
            getModeName buf.mode

        ( { edit, modeName }, continuation ) =
            P.parse buf.continuation key

        buf1 =
            updateMode modeName buf

        buf2 =
            edit
                |> Maybe.map
                    (\operator ->
                        case operator of
                            Move motion ->
                                buf1
                                    |> setCursor (runMotion motion buf1)

                            InsertString s ->
                                case buf1.mode of
                                    Ex linebuf ->
                                        buf1

                                    _ ->
                                        buf1
                                            |> Buf.transaction
                                                [ Insertion buf1.cursor s ]

                            _ ->
                                buf1
                    )
                |> Maybe.withDefault buf1

        buf3 =
            if oldModeName == modeName then
                buf2
            else
                modeChanged oldModeName modeName buf2

        buffers =
            Dict.insert buf3.id buf3 model.buffers
    in
        ( { model | buffers = buffers }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        PressKey bufid key ->
            model
                |> getBuffer bufid
                |> Maybe.map (handleKeypress key model)
                |> Maybe.withDefault ( model, Cmd.none )
