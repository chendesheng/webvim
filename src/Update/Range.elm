module Update.Range exposing (..)

import Model exposing (..)
import Vim.AST as V exposing (Operator(..))
import Internal.Position exposing (Position, positionMin)
import Internal.TextObject exposing (expandTextObject)
import Update.Motion exposing (..)
import Internal.TextBuffer as B


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


visualRegions :
    Bool
    -> V.VisualType
    -> Position
    -> Position
    -> B.TextBuffer
    -> List ( Position, Position )
visualRegions linewise tipe begin end lines =
    let
        b =
            min begin end

        e =
            max begin end

        ( ey, ex ) =
            e

        ( by, bx ) =
            b

        bx1 =
            min bx ex

        ex1 =
            max bx ex
    in
        if linewise then
            [ ( ( by, 0 )
              , ( ey + 1
                , 0
                )
              )
            ]
        else
            case tipe of
                V.VisualLine ->
                    [ ( ( by, 0 )
                      , ( ey + 1
                        , 0
                        )
                      )
                    ]

                V.VisualChars ->
                    [ ( b, ( ey, ex + 1 ) ) ]

                V.VisualBlock ->
                    List.range by ey
                        |> List.filterMap
                            (\i ->
                                lines
                                    |> B.getLine i
                                    |> Maybe.andThen
                                        (\line ->
                                            let
                                                maxcol =
                                                    B.lineMaxColumn line
                                            in
                                                if bx1 > maxcol then
                                                    Nothing
                                                else
                                                    Just
                                                        ( ( i, bx1 )
                                                        , ( i
                                                          , min (ex1 + 1) maxcol
                                                          )
                                                        )
                                        )
                            )


operatorRanges :
    Maybe Int
    -> V.OperatorRange
    -> Global
    -> Buffer
    -> List ( Position, Position )
operatorRanges count range global buf =
    case range of
        V.MotionRange md mo ->
            case runMotion count md mo global buf of
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
                    buf.lines
                        |> visualRegions linewise tipe begin end
                        |> List.reverse

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
                        buf.view.scrollTop
                        global.size.height
                        buf.syntax
                        textObject
                        around
                        buf.lines
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
