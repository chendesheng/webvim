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
        , syntaxHighlight
        , updateSavePoint
        , setShowTip
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
        , emptyBufferHistory
        , Mode
        , RegisterText
        , emptyBuffer
        , emptyView
        , View
        )
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
                    in
                        ( { buf
                            | lines = lines
                            , cursor = cursor
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
                , syntax = syntaxHighlight miny buf1.lines buf.syntax
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


syntaxHighlight : Int -> TextBuffer -> Syntax -> Syntax
syntaxHighlight start lines syntax =
    if syntax.lang == "" then
        syntax
    else
        let
            -- highlight.js doesn't support increment for now
            start =
                0

            ( slines, _ ) =
                foldlLines
                    start
                    (\line ( slines, continuation ) ->
                        let
                            sline =
                                highlight
                                    syntax.lang
                                    line
                                    continuation
                        in
                            ( Array.push sline slines
                            , Just (Tuple.second sline)
                            )
                    )
                    ( Array.empty, Nothing )
                    lines
        in
            { lang = syntax.lang
            , lines =
                Array.append (Array.slice 0 start syntax.lines)
                    slines
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
                        , history = undoHistory buf.history
                        , syntax = syntaxHighlight miny lines1 buf.syntax
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
                        , history = redoHistory buf.history
                        , syntax = syntaxHighlight miny lines1 buf.syntax
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


newBuffer : BufferInfo -> String -> Size -> Int -> Buffer
newBuffer info service size lineHeight =
    let
        { cursor, scrollTop, path, content } =
            info

        lines =
            content
                |> Maybe.withDefault ""
                |> fromString

        ( name, ext ) =
            filename path
                |> Debug.log "path"

        syntax =
            { lang =
                if ext == "" then
                    ""
                else
                    String.dropLeft 1 ext
            , lines = emptyBuffer.syntax.lines
            }
    in
        { emptyBuffer
            | lines = lines
            , config =
                { wordChars = ""
                , tabSize = 4
                , expandTab = True
                , lint = ext == ".elm"
                }
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
            , syntax =
                syntaxHighlight
                    0
                    lines
                    syntax
            , service = service
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
