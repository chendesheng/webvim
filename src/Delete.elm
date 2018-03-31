module Delete exposing (delete)

import Model exposing (..)
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Buffer as Buf
import Position exposing (Position, positionMin)
import Motion exposing (..)


type alias Transaction =
    { pos : Maybe ( Position, Bool )
    , patches : List Patch
    }


applyTransaction : Transaction -> Buffer -> Buffer
applyTransaction { pos, patches } buf =
    case pos of
        Just ( cursor, saveColumn ) ->
            buf
                |> Buf.setCursor cursor saveColumn
                |> Buf.transaction patches

        _ ->
            Buf.transaction patches buf


operatorRanges : V.OperatorRange -> Buffer -> List ( Position, Position )
operatorRanges range buf =
    case range of
        V.MotionRange md mo ->
            case runMotion md mo buf of
                Just pos ->
                    let
                        begin =
                            if pos > buf.cursor then
                                buf.cursor
                            else
                                pos

                        ( endy, endx ) =
                            if pos > buf.cursor then
                                pos
                            else
                                buf.cursor
                    in
                        if mo.linewise then
                            [ ( ( Tuple.first begin, 0 )
                              , ( if mo.inclusive then
                                    endy + 1
                                  else
                                    endy
                                , 0
                                )
                              )
                            ]
                        else
                            [ ( begin
                              , ( endy
                                , if mo.inclusive then
                                    endx + 1
                                  else
                                    endx
                                )
                              )
                            ]

                Nothing ->
                    []

        V.VisualRange ->
            case buf.mode of
                Visual tipe a b ->
                    let
                        begin =
                            min a b

                        end =
                            max a b

                        ( endy, endx ) =
                            end
                    in
                        case tipe of
                            VisualLine ->
                                [ ( ( Tuple.first begin, 0 )
                                  , ( endy + 1
                                    , 0
                                    )
                                  )
                                ]

                            _ ->
                                [ ( begin
                                  , ( endy
                                    , endx + 1
                                    )
                                  )
                                ]

                _ ->
                    []

        _ ->
            []


deleteOperator : V.OperatorRange -> Buffer -> Maybe Transaction
deleteOperator range buf =
    let
        ranges =
            operatorRanges range buf

        pos =
            case range of
                V.MotionRange md mo ->
                    case runMotion md mo buf of
                        Just pos ->
                            if mo.linewise then
                                Just ( pos, False )
                            else
                                Nothing

                        Nothing ->
                            Nothing

                V.VisualRange ->
                    case buf.mode of
                        Visual _ a b ->
                            let
                                begin =
                                    min a b
                            in
                                Just ( begin, True )

                        _ ->
                            Nothing

                _ ->
                    Nothing
    in
        if List.isEmpty ranges then
            Nothing
        else
            Just
                { pos = pos
                , patches =
                    List.map
                        (\( begin, end ) ->
                            Deletion begin end
                        )
                        ranges
                }


delete : String -> V.OperatorRange -> Buffer -> Buffer
delete register rg buf =
    case buf.mode of
        Ex prefix exbuf ->
            Buf.setMode
                ((case deleteOperator rg exbuf of
                    Just trans ->
                        applyTransaction trans exbuf

                    _ ->
                        exbuf
                 )
                    |> Ex prefix
                )
                buf

        Insert ->
            case deleteOperator rg buf of
                Just trans ->
                    applyTransaction trans buf

                _ ->
                    buf

        _ ->
            case deleteOperator rg buf of
                Just trans ->
                    buf
                        |> applyTransaction trans
                        |> saveLastDeleted register

                _ ->
                    buf


saveLastDeleted : String -> Buffer -> Buffer
saveLastDeleted reg buf =
    let
        s =
            buf
                |> Buf.getLastDeleted
                |> Maybe.map B.toString
                |> Maybe.withDefault ""
    in
        Buf.setRegister reg s buf
