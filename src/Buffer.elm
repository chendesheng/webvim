module Buffer
    exposing
        ( transaction
        , insert
        , delete
        , undo
        , redo
        , commit
        , clearHistory
        )

import Position exposing (..)
import Model exposing (Buffer, BufferHistory, emptyBufferHistory)
import Internal.TextBuffer
    exposing
        ( applyPatch
        , TextBuffer
        , getLine
        , lineBreak
        , fromString
        , countLines
        , Patch(..)
        , isEmpty
        )
import List
import Vim.AST
    exposing
        ( MotionData(..)
        , MotionOption
        , Direction(..)
        )
import String


applyPatches : List Patch -> TextBuffer -> ( TextBuffer, List Patch )
applyPatches patches lines =
    List.foldl
        (\patch ( lines, patches2 ) ->
            let
                ( patch2, lines2 ) =
                    applyPatch patch lines
            in
                ( lines2, patch2 :: patches2 )
        )
        ( lines, [] )
        patches


insert : Position -> String -> Buffer -> Buffer
insert pos s =
    transaction [ Insertion pos <| fromString s ]


delete : Position -> Position -> Buffer -> Buffer
delete from to =
    transaction [ Deletion from to ]


{-| batch edit text, keep cursor, save history
-}
transaction : List Patch -> Buffer -> Buffer
transaction patches buf =
    let
        ( buf1, undo ) =
            List.foldl
                (\patch ( buf, undo ) ->
                    let
                        ( patch1, lines ) =
                            applyPatch patch buf.lines

                        cursor =
                            updateCursor patch patch1 buf.cursor
                    in
                        ( { buf
                            | lines = lines
                            , cursor = cursor
                          }
                        , patch1 :: undo
                        )
                )
                ( buf, [] )
                patches
    in
        if List.isEmpty undo then
            buf
        else
            { buf1
                | history =
                    addPending buf.cursor undo buf1.history
            }


updateCursor : Patch -> Patch -> Position -> Position
updateCursor patch patch1 cursor =
    case patch of
        Insertion pos s ->
            if cursor < pos then
                cursor
            else
                case patch1 of
                    Deletion from to ->
                        positionSub to from
                            |> positionAdd cursor

                    _ ->
                        cursor

        Deletion from to ->
            case patch1 of
                Insertion _ s ->
                    if isEmpty s then
                        cursor
                    else if cursor < from then
                        cursor
                    else if from <= cursor && cursor < to then
                        from
                    else
                        positionSub to from
                            |> positionSub cursor

                Deletion _ _ ->
                    cursor


addPending : Position -> List Patch -> BufferHistory -> BufferHistory
addPending cursor patches history =
    history.pending
        |> Maybe.map
            (\pending ->
                let
                    pending1 =
                        { pending | patches = patches ++ pending.patches }
                in
                    { history | pending = Just pending1 }
            )
        |> Maybe.withDefault
            { history
                | pending = Just { cursor = cursor, patches = patches }
            }


{-| add pending changes to undo ist and clear redo list
-}
commit : Buffer -> Buffer
commit buf =
    if buf.history.pending == Nothing then
        buf
    else
        { buf
            | history =
                let
                    history =
                        buf.history
                in
                    history.pending
                        |> Maybe.map
                            (\pending ->
                                { history
                                    | undoes = pending :: history.undoes
                                    , pending = Nothing
                                    , redoes = []
                                }
                            )
                        |> Maybe.withDefault history
        }


{-| undo last change
-}
undo : Buffer -> Buffer
undo buf =
    buf.history.undoes
        |> List.head
        |> Maybe.map
            (\{ cursor, patches } ->
                let
                    ( lines1, patches1 ) =
                        applyPatches patches buf.lines

                    undoHistory { undoes, redoes } =
                        { undoes =
                            List.tail undoes
                                |> Maybe.withDefault []
                        , pending = Nothing
                        , redoes =
                            { cursor = buf.cursor
                            , patches = patches1
                            }
                                :: redoes
                        }
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
    buf.history.redoes
        |> List.head
        |> Maybe.map
            (\{ cursor, patches } ->
                let
                    ( lines1, patches1 ) =
                        applyPatches patches buf.lines

                    redoHistory { undoes, redoes } =
                        { undoes =
                            { cursor = buf.cursor
                            , patches = patches1
                            }
                                :: undoes
                        , pending = Nothing
                        , redoes =
                            List.tail redoes
                                |> Maybe.withDefault []
                        }
                in
                    { buf
                        | lines = lines1
                        , cursor = cursor
                        , history = redoHistory buf.history
                    }
            )
        |> Maybe.withDefault buf


moveByClass : MotionData -> MotionOption -> Buffer -> Buffer
moveByClass class option buf =
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


clearHistory : Buffer -> Buffer
clearHistory buf =
    { buf | history = emptyBufferHistory }
