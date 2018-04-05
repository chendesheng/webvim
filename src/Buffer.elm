module Buffer
    exposing
        ( transaction
        , insert
        , delete
        , undo
        , redo
        , commit
        , clearHistory
        , getLastDeleted
        , setMode
        , setRegister
        , setCursor
        , putString
        , syntaxHighlight
        )

import Position exposing (..)
import Model exposing (Buffer, BufferHistory, emptyBufferHistory, Mode)
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
        , foldlLines
        )
import List
import Vim.AST
    exposing
        ( MotionData(..)
        , MotionOption
        , Direction(..)
        )
import String
import Maybe
import Dict
import Syntax exposing (..)
import Array


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
                , syntax = syntaxHighlight buf1.lines buf.syntax
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


syntaxHighlight : TextBuffer -> Syntax -> Syntax
syntaxHighlight lines syntax =
    if syntax.lang == "" then
        syntax
    else
        let
            ( slines, _ ) =
                foldlLines
                    0
                    (\line ( slines, continuation ) ->
                        let
                            sline =
                                highlight
                                    syntax.lang
                                    line
                                    continuation
                        in
                            ( Array.append slines <|
                                Array.fromList [ sline ]
                            , Just (Tuple.second sline)
                            )
                    )
                    ( Array.empty, Nothing )
                    lines
        in
            { lang = syntax.lang
            , lines = slines
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
                        , cursorColumn = Tuple.second cursor
                        , history = undoHistory buf.history
                        , syntax = syntaxHighlight lines1 buf.syntax
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
                        , cursorColumn = Tuple.second cursor
                        , history = redoHistory buf.history
                        , syntax = syntaxHighlight lines1 buf.syntax
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


getLastPatch : Buffer -> Maybe Patch
getLastPatch { history } =
    history.pending
        |> Maybe.andThen
            (\undo ->
                -- TODO: merge patchs
                undo.patches
                    |> List.head
            )


getLastDeleted : Buffer -> Maybe TextBuffer
getLastDeleted buf =
    getLastPatch buf
        |> Maybe.andThen
            (\patch ->
                case patch of
                    Insertion _ s ->
                        Just s

                    Deletion _ _ ->
                        Nothing
            )


setRegister : String -> String -> Buffer -> Buffer
setRegister reg val buf =
    { buf | registers = Dict.insert reg val buf.registers }


setMode : Mode -> Buffer -> Buffer
setMode mode buf =
    { buf | mode = mode }


setCursor : Position -> Bool -> Buffer -> Buffer
setCursor cursor saveColumn buf =
    { buf
        | cursor = cursor
        , cursorColumn =
            if saveColumn then
                Tuple.second cursor
            else
                buf.cursorColumn
    }


putString : Bool -> String -> Buffer -> Buffer
putString forward s buf =
    let
        ( y, x ) =
            buf.cursor

        forward1 =
            case getLine y buf.lines of
                Just line ->
                    -- always put backward when line is empty
                    if line == lineBreak then
                        False
                    else
                        forward

                _ ->
                    forward

        buf1 =
            transaction
                [ if forward1 then
                    Insertion ( y, x + 1 ) <| fromString s
                  else
                    Insertion ( y, x ) <| fromString s
                ]
                buf

        cursor =
            case getLastPatch buf1 of
                Just patch ->
                    case patch of
                        Deletion _ to ->
                            to

                        _ ->
                            buf.cursor

                _ ->
                    buf.cursor
    in
        setCursor cursor True buf1
