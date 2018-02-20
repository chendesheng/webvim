module Buffer exposing (insert, delete, undo, redo)

import Types exposing (..)
import Model exposing (Buffer)
import Internal.TextBuffer exposing (applyPatch, TextBuffer, getLine)
import List
import Vim.AST exposing (PositionClass(..), Direction(..))


{-| insert text, keep cursor, save history
-}
insert : Position -> String -> Buffer -> Buffer
insert pos s buf =
    let
        ( patch, lines ) =
            applyPatch (Insertion pos s) buf.lines

        undo =
            { cursor = buf.cursor, patch = patch }

        cursor =
            if buf.cursor <= pos then
                buf.cursor
            else
                case patch of
                    Deletion from to ->
                        positionSub to from
                            |> positionAdd buf.cursor

                    _ ->
                        buf.cursor
    in
        { buf
            | lines = lines
            , cursor = cursor
            , history = buf.history |> Tuple.mapFirst ((::) undo)
        }


{-| delete range of text, right end exclusive
-}
delete : Position -> Position -> Buffer -> Buffer
delete from to buf =
    let
        ( patch, lines ) =
            applyPatch (Deletion from to) buf.lines

        undo =
            { cursor = buf.cursor, patch = patch }

        cursor =
            if buf.cursor < from then
                buf.cursor
            else if from <= buf.cursor && buf.cursor < to then
                from
            else
                case patch of
                    Insertion _ _ ->
                        positionSub to from
                            |> positionSub buf.cursor

                    _ ->
                        buf.cursor
    in
        { buf
            | lines = lines
            , cursor = cursor
            , history = buf.history |> Tuple.mapFirst ((::) undo)
        }


{-| undo last change
-}
undo : Buffer -> Buffer
undo buf =
    buf.history
        |> Tuple.first
        |> List.head
        |> Maybe.map
            (\{ cursor, patch } ->
                let
                    ( patch1, lines1 ) =
                        applyPatch patch buf.lines

                    undoHistory ( undoes, redoes ) =
                        ( List.tail undoes |> Maybe.withDefault []
                        , { cursor = buf.cursor
                          , patch = patch1
                          }
                            :: redoes
                        )
                in
                    { buf
                        | lines = lines1
                        , cursor = cursor
                        , history = undoHistory buf.history
                    }
            )
        |> Maybe.withDefault buf


{-| redo
-}
redo : Buffer -> Buffer
redo buf =
    buf.history
        |> Tuple.second
        |> List.head
        |> Maybe.map
            (\{ cursor, patch } ->
                let
                    ( patch1, lines1 ) =
                        applyPatch patch buf.lines

                    redoHistory ( undoes, redoes ) =
                        ( { cursor = buf.cursor
                          , patch = patch1
                          }
                            :: undoes
                        , List.tail redoes |> Maybe.withDefault []
                        )
                in
                    { buf
                        | lines = lines1
                        , cursor = cursor
                        , history = redoHistory buf.history
                    }
            )
        |> Maybe.withDefault buf


findPositionClass : String -> PositionClass -> String -> Maybe Int
findPositionClass wordChars class line =
    case class of
        WordStart ->
            -- wordChar*notWordChars+wordChars+
            Just 0

        _ ->
            Nothing


moveByClass : PositionClass -> Direction -> Buffer -> Buffer
moveByClass class direction buf =
    let
        ( y, x ) =
            buf.cursor
    in
        buf.lines
            |> getLine y
            |> Maybe.map
                (\line ->
                    let
                        line1 =
                            String.dropLeft x line
                    in
                        buf
                )
            |> Maybe.withDefault buf
