module Update.Select exposing (select)

import Vim.AST as V exposing (Operator(..), ChangeCase(..))
import Model exposing (..)
import Update.Range exposing (operatorRanges, shrinkRight)
import Update.Buffer as Buf
import Internal.TextObject exposing (expandTextObject)
import Update.Motion exposing (..)


select : Maybe Int -> V.TextObject -> Bool -> Buffer -> Buffer
select count textobj around buf =
    case buf.mode of
        Visual { tipe, begin, end } ->
            (if begin == end then
                begin
             else if buf.cursor == max begin end then
                Tuple.mapSecond ((+) 1) (max begin end)
             else
                Tuple.mapSecond (flip (-) 1) (min begin end)
            )
                |> expandTextObject buf.config.wordChars
                    buf.view.scrollTop
                    buf.view.size.height
                    buf.syntax
                    textobj
                    around
                    buf.lines
                |> Maybe.map
                    (\rg ->
                        let
                            ( a, b ) =
                                shrinkRight rg

                            begin1 =
                                if begin == end then
                                    min a b
                                else
                                    a
                                        |> min b
                                        |> min begin
                                        |> min end

                            end1 =
                                if begin == end then
                                    max a b
                                else
                                    a
                                        |> max b
                                        |> max begin
                                        |> max end
                        in
                            if
                                (buf.cursor == min begin end)
                                    && (begin /= end)
                            then
                                buf
                                    |> Buf.setCursor begin1 True
                                    |> setVisualEnd begin1
                                    |> setVisualBegin end1
                            else
                                buf
                                    |> Buf.setCursor end1 True
                                    |> setVisualEnd end1
                                    |> setVisualBegin begin1
                    )
                |> Maybe.withDefault buf

        _ ->
            buf
