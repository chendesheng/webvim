module Update.Select exposing (select)

import Internal.Position exposing (excludeRight)
import Internal.TextObject exposing (expandTextObject)
import Model exposing (..)
import Model.Buffer exposing (..)
import Model.Global exposing (..)
import Model.Lint exposing (..)
import Model.View as View
import Update.Buffer as Buf
import Update.Motion exposing (..)
import Vim.AST as V exposing (ChangeCase(..), Operator(..))


select : Maybe Int -> V.TextObject -> Bool -> Editor -> Editor
select count textobj around ({ buf } as ed) =
    case buf.mode of
        Visual { begin, end } ->
            { ed
                | buf =
                    (if begin == end then
                        begin

                     else if buf.view.cursor == max begin end then
                        Tuple.mapSecond ((+) 1) (max begin end)

                     else
                        Tuple.mapSecond (\n -> n - 1) (min begin end)
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
                                        excludeRight rg

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
                                    (buf.view.cursor == min begin end)
                                        && (begin /= end)
                                then
                                    buf
                                        |> Buf.updateView (View.setCursor begin1 True)
                                        |> setVisualEnd begin1
                                        |> setVisualBegin end1

                                else
                                    buf
                                        |> Buf.updateView (View.setCursor end1 True)
                                        |> setVisualEnd end1
                                        |> setVisualBegin begin1
                            )
                        |> Maybe.withDefault buf
            }

        _ ->
            ed
