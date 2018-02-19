module Buffer exposing (insert, delete, undo, redo)

import Types exposing (..)
import Model exposing (Buffer)
import Internal.TextBuffer exposing (applyPatch, TextBuffer)


{-| insert text, keep cursor, save history
-}
insert : Position -> String -> Buffer -> Buffer
insert pos s buf =
    let
        ( patch, lines ) =
            applyPatch (Insertion pos s) buf.lines

        undo =
            { cursor = buf.cursor, patch = patch }

        ( undoes, redoes ) =
            buf.history

        cursor =
            if buf.cursor <= pos then
                buf.cursor
            else
                case patch of
                    Deletion _ end ->
                        end

                    _ ->
                        buf.cursor
    in
        { buf
            | lines = lines
            , cursor = cursor
            , history = ( undo :: undoes, redoes )
        }


{-| delete range of text
-}
delete : Position -> Position -> Buffer -> Buffer
delete pos pos2 buf =
    buf


{-| undo last change
-}
undo : Buffer -> Buffer
undo buf =
    buf


{-| redo
-}
redo : Buffer -> Buffer
redo buf =
    buf
