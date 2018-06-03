module Update.Range exposing (..)

import Model exposing (..)
import Vim.AST as V exposing (Operator(..))
import Internal.Position exposing (Position, positionMin)
import Internal.TextObject exposing (expandTextObject)
import Update.Motion exposing (..)


isLinewise : V.OperatorRange -> Mode -> Bool
isLinewise range mode =
    case range of
        V.MotionRange md mo ->
            mo.linewise

        V.VisualRange linewise ->
            case mode of
                Visual { tipe, begin, end } ->
                    (case tipe of
                        V.VisualLine ->
                            True

                        V.VisualBlock ->
                            False

                        _ ->
                            linewise
                    )

                _ ->
                    False

        V.TextObject textObject around ->
            textObject == V.Line && around


operatorRanges : Maybe Int -> V.OperatorRange -> Buffer -> List ( Position, Position )
operatorRanges count range buf =
    case range of
        V.MotionRange md mo ->
            case runMotion count md mo buf of
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

        V.VisualRange linewise ->
            case buf.mode of
                Visual { tipe, begin, end } ->
                    let
                        begin1 =
                            min begin end

                        end1 =
                            max begin end

                        ( endy, endx ) =
                            end1

                        getRange linewise =
                            if linewise then
                                [ ( ( Tuple.first begin1, 0 )
                                  , ( endy + 1
                                    , 0
                                    )
                                  )
                                ]
                            else
                                [ ( begin1
                                  , ( endy
                                    , endx + 1
                                    )
                                  )
                                ]
                    in
                        getRange
                            (case tipe of
                                V.VisualLine ->
                                    True

                                V.VisualBlock ->
                                    False

                                _ ->
                                    linewise
                            )

                _ ->
                    []

        V.TextObject textObject around ->
            let
                ( y, x ) =
                    buf.cursor
            in
                buf.cursor
                    |> expandTextObject
                        buf.config.wordChars
                        textObject
                        around
                        buf.lines
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
