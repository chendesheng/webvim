module Update.Buffer
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
        , updateSavePoint
        , setShowTip
        , isDirty
        , isEditing
        , configs
        , indentCursorToLineFirst
        , setScrollTop
        , updateView
        , bestScrollTop
        , toWords
        )

import Internal.Position exposing (..)
import Internal.PositionClass exposing (findLineFirst)
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
        , Patch(..)
        , isEmpty
        , foldlLines
        , shiftPositionByPatch
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
import Internal.Syntax
    exposing
        ( Syntax
        , Token
        , applyPatchToSyntax
        , applyPatchesToSyntax
        , splitTokens
        )
import Elm.Array as Array exposing (Array)
import Internal.Jumps exposing (applyPatchesToJumps, applyPatchesToLocations)
import Update.Message exposing (LintError)
import Helper.Helper exposing (parseWords)


applyPatchToLintError : Patch -> LintError -> LintError
applyPatchToLintError patch ({ region, subRegion } as error) =
    let
        updateRegion ( b, e ) =
            ( shiftPositionByPatch patch b
            , shiftPositionByPatch patch e
            )
    in
        { error
            | region = updateRegion region
            , subRegion = Maybe.map updateRegion subRegion
        }


applyPatchesToLintErrors : List LintError -> List Patch -> List LintError
applyPatchesToLintErrors =
    List.foldl
        (\patch result ->
            List.map
                (applyPatchToLintError patch)
                result
        )


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
        ( buf1, undo ) =
            List.foldl
                (\patch ( buf, undo ) ->
                    let
                        ( patch1, lines ) =
                            applyPatch patch buf.lines

                        cursor =
                            updateCursor patch patch1 buf.cursor

                        ( syntax, _ ) =
                            applyPatchToSyntax patch buf.syntax

                        ( y, _ ) =
                            cursor
                    in
                        ( { buf
                            | lines = lines
                            , cursor = cursor
                            , syntax = syntax
                            , syntaxDirtyFrom =
                                min
                                    buf.syntaxDirtyFrom
                                    y
                          }
                        , patch1 :: undo
                        )
                )
                ( buf, [] )
                patches

        --_ =
        --    Debug.log "patches" patches
        --_ =
        --    Debug.log "undo" undo
    in
        if List.isEmpty undo then
            buf
        else
            let
                history =
                    addPending undo buf1.history

                --_ =
                --    Debug.log "history" history
            in
                { buf1
                    | history = { history | version = history.version + 1 }
                    , jumps = applyPatchesToJumps patches buf.jumps
                    , lint =
                        { items =
                            applyPatchesToLintErrors
                                buf.lint.items
                                patches

                        --|> Debug.log "update lint.items"
                        , count = buf.lint.count
                        }
                    , locationList =
                        applyPatchesToLocations
                            buf.locationList
                            patches
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


addPending : List Patch -> BufferHistory -> BufferHistory
addPending patches history =
    { history
        | pending =
            List.foldl
                (\patch result ->
                    case result of
                        x :: xs ->
                            case B.mergePatch patch x of
                                Just patch1 ->
                                    patch1 :: xs

                                _ ->
                                    patch :: x :: xs

                        _ ->
                            [ patch ]
                )
                history.pending
                (List.reverse patches)
    }


{-| add pending changes to undo ist and clear redo list
-}
commit : Buffer -> Buffer
commit buf =
    let
        history =
            buf.history

        { pending, undoes } =
            history
    in
        case pending of
            [] ->
                buf

            _ ->
                { buf
                    | history =
                        { history
                            | undoes = pending :: undoes
                            , pending = []
                            , redoes = []
                        }
                }


{-| undo last change
-}
undo : Buffer -> Buffer
undo buf =
    buf.history.undoes
        |> List.head
        |> Maybe.map
            (\undo ->
                let
                    history =
                        buf.history

                    ( lines1, patches1, miny ) =
                        applyPatches undo buf.lines

                    ( syntax, n ) =
                        applyPatchesToSyntax undo buf.syntax

                    jumps =
                        applyPatchesToJumps undo buf.jumps

                    lintErrors =
                        applyPatchesToLintErrors
                            buf.lint.items
                            undo

                    undoHistory { undoes, redoes } =
                        { history
                            | undoes =
                                List.tail undoes
                                    |> Maybe.withDefault []
                            , pending = []
                            , redoes = patches1 :: redoes
                            , version = history.version + 1
                        }

                    cursor =
                        patches1
                            |> List.map B.patchCursor
                            |> List.minimum
                            |> Maybe.withDefault buf.cursor

                    ( y, x ) =
                        cursor
                in
                    { buf
                        | lines = lines1
                        , cursor = cursor
                        , cursorColumn = x
                        , syntax = syntax
                        , syntaxDirtyFrom = y
                        , history = undoHistory buf.history
                        , jumps = jumps
                        , lint =
                            { items = lintErrors
                            , count = buf.lint.count
                            }
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
            (\redo ->
                let
                    history =
                        buf.history

                    ( lines1, patches1, miny ) =
                        applyPatches redo buf.lines

                    ( syntax, n ) =
                        applyPatchesToSyntax redo buf.syntax

                    jumps =
                        applyPatchesToJumps redo buf.jumps

                    lintErrors =
                        applyPatchesToLintErrors buf.lint.items redo

                    redoHistory { undoes, redoes } =
                        { history
                            | undoes = patches1 :: undoes
                            , pending = []
                            , redoes =
                                List.tail redoes
                                    |> Maybe.withDefault []
                            , version = history.version + 1
                        }

                    cursor =
                        patches1
                            |> List.map B.patchCursor
                            |> List.minimum
                            |> Maybe.withDefault buf.cursor

                    ( y, x ) =
                        cursor
                in
                    { buf
                        | lines = lines1
                        , cursor = cursor
                        , cursorColumn = x
                        , syntax = syntax
                        , syntaxDirtyFrom = y
                        , history = redoHistory buf.history
                        , jumps = jumps
                        , lint =
                            { items = lintErrors
                            , count = buf.lint.count
                            }
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
    List.head history.pending


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
                            case buf.mode of
                                Model.Insert _ ->
                                    ( Insertion ( y, x ) <| fromString s
                                    , Nothing
                                    )

                                _ ->
                                    ( Insertion ( y, 0 ) <| fromString s
                                    , Just ( y, findLineFirst s + 1 )
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
    buf.history.savePoint /= buf.history.version


isEditing : Buffer -> Buffer -> Bool
isEditing buf1 buf2 =
    buf1.history.version
        /= buf2.history.version


indentCursorToLineFirst : Buffer -> Buffer
indentCursorToLineFirst buf =
    let
        ( y, x ) =
            buf.cursor
    in
        if x == 0 then
            setCursor
                ( y
                , buf.lines
                    |> B.getLine y
                    |> Maybe.map findLineFirst
                    |> Maybe.withDefault 0
                )
                True
                buf
        else
            buf


updateView : (View -> View) -> Buffer -> Buffer
updateView f buf =
    let
        view =
            buf.view
    in
        { buf | view = f buf.view }


setScrollTop : Int -> Buffer -> Buffer
setScrollTop n buf =
    updateView (\v -> { v | scrollTop = n }) buf


bestScrollTop : Int -> Int -> B.TextBuffer -> Int -> Int
bestScrollTop y height lines scrollTop =
    let
        maxLine =
            B.count lines - 1
    in
        if scrollTop <= y && y < scrollTop + height then
            scrollTop
        else if y >= maxLine - height then
            maxLine - height
        else
            y
                - (height // 2)
                |> max 0


toWords : String -> Buffer -> List String
toWords exclude { config, lines } =
    lines
        |> B.mapLines (parseWords config.wordChars)
        |> Array.toList
        |> List.concat
        |> List.foldl
            (\word ->
                Dict.update word
                    (\cnt ->
                        cnt
                            |> Maybe.map ((+) 1)
                            |> Maybe.withDefault 1
                            |> Just
                    )
            )
            Dict.empty
        |> Dict.update
            exclude
            (Maybe.andThen
                (\n ->
                    if n > 1 then
                        Just (n - 1)
                    else
                        Nothing
                )
            )
        |> Dict.toList
        |> List.sortWith
            (\( ka, va ) ( kb, vb ) ->
                if va > vb then
                    LT
                else if va < vb then
                    GT
                else
                    let
                        la =
                            String.length ka

                        lb =
                            String.length kb
                    in
                        if la > lb then
                            GT
                        else if la < lb then
                            LT
                        else
                            EQ
            )
        |> List.map Tuple.first