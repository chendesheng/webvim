module Buffer
    exposing
        ( transaction
        , newBuffer
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
        , updateSavePoint
        , setShowTip
        , isDirty
        , isEditing
        )

import Window exposing (Size)
import Regex as Re
import Position exposing (..)
import PositionClass exposing (findLineFirst)
import Message exposing (BufferInfo)
import Model
    exposing
        ( Buffer
        , BufferHistory
        , BufferConfig
        , emptyBufferHistory
        , defaultBufferConfig
        , Mode
        , RegisterText
        , emptyBuffer
        , emptyView
        , View
        )
import Internal.TextBuffer as B
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
import Dict exposing (Dict)
import Syntax
    exposing
        ( Syntax
        , Token
        , applyPatchToSyntax
        , applyPatchesToSyntax
        )
import Elm.Array as Array exposing (Array)
import Helper exposing (minMaybe)


applyPatches : List Patch -> TextBuffer -> ( TextBuffer, List Patch, Int )
applyPatches patches lines =
    List.foldl
        (\patch ( lines, patches2, miny ) ->
            let
                ( patch2, lines2 ) =
                    applyPatch patch lines
            in
                ( lines2, patch2 :: patches2, min miny (minLine patch) )
        )
        ( lines, [], 0xFFFFFFFF )
        patches


insert : Position -> String -> Buffer -> Buffer
insert pos s =
    transaction [ Insertion pos <| fromString s ]


delete : Position -> Position -> Buffer -> Buffer
delete from to =
    transaction [ Deletion from to ]


minLine : Patch -> Int
minLine patch =
    case patch of
        Insertion ( y, _ ) _ ->
            y

        Deletion ( y1, _ ) ( y2, _ ) ->
            min y1 y2


arrayLast : Array a -> Maybe a
arrayLast arr =
    Array.get (Array.length arr - 1) arr


{-| batch edit text, keep cursor, save history
-}
transaction : List Patch -> Buffer -> Buffer
transaction patches buf =
    let
        ( buf1, undo, miny ) =
            List.foldl
                (\patch ( buf, undo, miny ) ->
                    let
                        ( patch1, lines ) =
                            applyPatch patch buf.lines

                        cursor =
                            updateCursor patch patch1 buf.cursor

                        ( syntax, dirtyFrom ) =
                            applyPatchToSyntax patch buf.syntax
                    in
                        ( { buf
                            | lines = lines
                            , cursor = cursor
                            , syntax = syntax
                            , syntaxDirtyFrom =
                                minMaybe
                                    buf.syntaxDirtyFrom
                                    dirtyFrom
                          }
                        , patch1 :: undo
                        , min miny (minLine patch)
                        )
                )
                ( buf, [], 0x000000FFFFFFFFFF )
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
    case history.pending of
        Just pending ->
            let
                pending1 =
                    { pending | patches = patches ++ pending.patches }
            in
                { history | pending = Just pending1 }

        _ ->
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
                    case history.pending of
                        Just pending ->
                            { history
                                | undoes = pending :: history.undoes
                                , pending = Nothing
                                , redoes = []
                                , version = history.version + 1
                            }

                        _ ->
                            history
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
                    history =
                        buf.history

                    ( lines1, patches1, miny ) =
                        applyPatches patches buf.lines

                    ( syntax, n ) =
                        applyPatchesToSyntax patches buf.syntax

                    undoHistory { undoes, redoes } =
                        { history
                            | undoes =
                                List.tail undoes
                                    |> Maybe.withDefault []
                            , pending = Nothing
                            , redoes =
                                { cursor = buf.cursor
                                , patches = patches1
                                }
                                    :: redoes
                            , version =
                                case undoes of
                                    [] ->
                                        history.version

                                    _ ->
                                        history.version - 1
                        }
                in
                    { buf
                        | lines = lines1
                        , cursor = cursor
                        , cursorColumn = Tuple.second cursor
                        , syntax = syntax
                        , syntaxDirtyFrom = minMaybe buf.syntaxDirtyFrom n
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
                    history =
                        buf.history

                    ( lines1, patches1, miny ) =
                        applyPatches patches buf.lines

                    ( syntax, n ) =
                        applyPatchesToSyntax patches buf.syntax

                    redoHistory { undoes, redoes } =
                        { history
                            | undoes =
                                { cursor = buf.cursor
                                , patches = patches1
                                }
                                    :: undoes
                            , pending = Nothing
                            , redoes =
                                List.tail redoes
                                    |> Maybe.withDefault []
                            , version =
                                case redoes of
                                    [] ->
                                        history.version

                                    _ ->
                                        history.version + 1
                        }
                in
                    { buf
                        | lines = lines1
                        , cursor = cursor
                        , cursorColumn = Tuple.second cursor
                        , syntax = syntax
                        , syntaxDirtyFrom = minMaybe buf.syntaxDirtyFrom n
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


setRegister : String -> RegisterText -> Buffer -> Buffer
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


putString : Bool -> RegisterText -> Buffer -> Buffer
putString forward text buf =
    let
        ( y, x ) =
            buf.cursor

        ( patch, cursor ) =
            let
                line =
                    getLine y buf.lines |> Maybe.withDefault ""
            in
                case text of
                    Model.Text s ->
                        if forward && line /= lineBreak then
                            ( Insertion ( y, x + 1 ) <| fromString s
                            , Nothing
                            )
                        else
                            ( Insertion ( y, x ) <| fromString s
                            , Nothing
                            )

                    Model.Lines s ->
                        if forward then
                            let
                                s1 =
                                    if
                                        String.endsWith
                                            lineBreak
                                            line
                                    then
                                        s
                                    else
                                        lineBreak ++ s
                            in
                                ( Insertion ( y + 1, 0 ) <| fromString s1
                                , Just ( y + 1, findLineFirst s + 1 )
                                )
                        else
                            (if buf.mode == Model.Insert then
                                ( Insertion ( y, x ) <| fromString s
                                , Nothing
                                )
                             else
                                ( Insertion ( y, 0 ) <| fromString s
                                , Just ( y, findLineFirst s + 1 )
                                )
                            )

        getLastDeletedTo buf =
            case getLastPatch buf of
                Just patch ->
                    case patch of
                        Deletion _ to ->
                            to

                        _ ->
                            buf.cursor

                _ ->
                    buf.cursor
    in
        buf
            |> transaction [ patch ]
            |> (\buf1 ->
                    setCursor
                        (case cursor of
                            Just p ->
                                p

                            Nothing ->
                                getLastDeletedTo buf1
                        )
                        True
                        buf1
               )


filename : String -> ( String, String )
filename s =
    case
        Re.find
            (Re.AtMost 1)
            (Re.regex "(^|[/\\\\])([^.]+)([.][^.]*)?$")
            s
    of
        [ m ] ->
            case m.submatches of
                [ _, a, b ] ->
                    ( Maybe.withDefault "" a
                    , Maybe.withDefault "" b
                    )

                _ ->
                    ( "", "" )

        _ ->
            ( "", "" )


configs : Dict String BufferConfig
configs =
    Dict.fromList
        [ ( ".elm"
          , { defaultBufferConfig
                | tabSize = 4
                , lint = True
            }
          )
        ]


newBuffer : BufferInfo -> String -> String -> Size -> Int -> Buffer
newBuffer info service syntaxService size lineHeight =
    let
        { cursor, scrollTop, path, content } =
            info

        lines =
            content
                |> Maybe.withDefault ""
                |> fromString

        ( name, ext ) =
            filename path

        --|> Debug.log "path"
    in
        { emptyBuffer
            | lines = lines
            , config =
                configs
                    |> Dict.get ext
                    |> Maybe.withDefault defaultBufferConfig
            , view =
                { emptyView
                    | size = size
                    , lineHeight = lineHeight
                    , scrollTop = scrollTop
                }
            , cursor = cursor
            , cursorColumn = Tuple.second cursor
            , path = path
            , name = name ++ ext
            , service = service
            , syntaxService = syntaxService
        }


updateSavePoint : Buffer -> Buffer
updateSavePoint buf =
    let
        history =
            buf.history
    in
        { buf
            | history = { history | savePoint = history.version }
        }


setShowTip : Bool -> Buffer -> Buffer
setShowTip showTip buf =
    let
        view =
            buf.view
    in
        { buf | view = { view | showTip = showTip } }


isDirty : Buffer -> Bool
isDirty buf =
    let
        history =
            buf.history
    in
        history.pending /= Nothing || history.savePoint /= history.version


isEditing : Buffer -> Buffer -> Bool
isEditing buf1 buf2 =
    let
        h1 =
            buf1.history

        h2 =
            buf2.history

        pendingPatches pending =
            case pending of
                Just { patches } ->
                    List.length patches

                _ ->
                    0
    in
        h1.version
            /= h2.version
            || pendingPatches h1.pending
            /= pendingPatches h2.pending
