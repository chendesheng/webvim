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
        , setShowTip
        , isDirty
        , isEditing
        , configs
        , indentCursorToLineFirst
        , setScrollTop
        , updateView
        , bestScrollTop
        , toWords
        , cursorLineFirst
        , gotoLine
        , setLastIndent
        , cancelLastIndent
        , setCursorColumn
        , infoMessage
        , errorMessage
        , getStatusBar
        , clearMessage
        , cIndentRules
        , finalScrollTop
        , getViewLines
        , scrollViewLines
        , fillEmptyViewLines
        , switchVisualEnd
        , shortPath
        , updateHistory
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
        , Mode(..)
        , RegisterText
        , emptyBuffer
        , emptyView
        , View
        , LintError
        , emptyUndo
        , IndentConfig(..)
        , StatusMessage(..)
        , ExPrefix(..)
        , ViewLine
        , IME
        , emptyIme
        )
import Internal.TextBuffer as B
    exposing
        ( applyPatch
        , TextBuffer
        , getLine
        , lineBreak
        , fromString
        , fromStringExpandTabs
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
        , VisualType(..)
        , ModeName(..)
        )
import String
import Maybe
import Dict exposing (Dict)
import Internal.Syntax
    exposing
        ( Syntax
        , Token
        , applyPatchToSyntax
        , splitTokens
        )
import Array as Array exposing (Array)
import Internal.Jumps exposing (applyPatchesToJumps, applyPatchesToLocations)
import Helper.Helper exposing (parseWords, relativePath, regex)
import Regex as Re


applyPatchToLintError : Patch -> LintError -> Maybe LintError
applyPatchToLintError patch ({ region, subRegion } as error) =
    let
        updateRegion ( b, e ) =
            case patch of
                Deletion bDel eDel ->
                    if bDel <= b && e <= eDel then
                        Nothing
                    else
                        Just
                            ( shiftPositionByPatch patch b
                            , shiftPositionByPatch patch e
                            )

                Insertion pos _ ->
                    Just
                        ( shiftPositionByPatch patch b
                        , shiftPositionByPatch patch e
                        )

        maybeRegion =
            updateRegion region

        maybeSubRegion =
            Maybe.andThen updateRegion subRegion
    in
        maybeRegion
            |> Maybe.map
                (\rg ->
                    { error
                        | region = rg
                        , subRegion = maybeSubRegion
                    }
                )


applyPatchesToLintErrors : List LintError -> List Patch -> List LintError
applyPatchesToLintErrors =
    List.foldl
        (\patch result ->
            List.filterMap
                (applyPatchToLintError patch)
                result
        )


applyPatches :
    List Patch
    -> Buffer
    ->
        { lines : TextBuffer
        , patches : List Patch
        , syntax : Syntax
        , viewLines : List (Maybe ViewLine)
        }
applyPatches patches buf =
    let
        lines =
            buf.lines

        view =
            buf.view

        scrollTop =
            finalScrollTop buf
    in
        List.foldl
            (\patch args ->
                let
                    ( patch2, lines2 ) =
                        applyPatch patch args.lines

                    syntax2 =
                        Tuple.first <| applyPatchToSyntax patch args.syntax
                in
                    { lines = lines2
                    , patches = patch2 :: args.patches
                    , syntax = syntax2
                    , viewLines =
                        applyPatchToViewLines
                            scrollTop
                            view.size.height
                            patch2
                            lines
                            lines2
                            syntax2
                            args.viewLines
                    }
            )
            { lines = lines
            , patches = []
            , syntax = buf.syntax
            , viewLines = view.lines
            }
            patches


{-| for unit testing
-}
insert : Position -> String -> Buffer -> Buffer
insert pos s =
    transaction [ Insertion pos <| fromString s ]


{-| for unit testing
-}
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


getViewLines :
    Int
    -> Int
    -> B.TextBuffer
    -> Syntax
    -> List (Maybe ViewLine)
getViewLines begin end lines syntax =
    if begin > end then
        []
    else
        let
            holes =
                List.repeat (end - B.count lines) Nothing
        in
            B.indexedMapLinesToList
                begin
                end
                (\i s ->
                    Just
                        { lineNumber = i
                        , text = s
                        , tokens =
                            Array.get i syntax
                                |> Maybe.withDefault []
                        }
                )
                lines
                ++ holes


max3 : Int -> Int -> Int -> Int
max3 a b =
    max b >> max a


patchRegion : Patch -> ( Position, Position )
patchRegion patch =
    case patch of
        Insertion ( by, bx ) s ->
            let
                n =
                    B.count s - 1

                ex_ =
                    B.getLine n s
                        |> Maybe.withDefault ""
                        |> String.length

                ex =
                    if n == 0 then
                        bx + ex_
                    else
                        ex_
            in
                ( ( by, bx )
                , ( by + n, ex )
                )

        Deletion b e ->
            ( b, e )


applyPatchToViewLines :
    Int
    -> Int
    -> Patch
    -> B.TextBuffer
    -> B.TextBuffer
    -> Syntax
    -> List (Maybe ViewLine)
    -> List (Maybe ViewLine)
applyPatchToViewLines scrollTop height_ patch oldLines lines syntax viewLines =
    let
        height =
            height_ + 2

        --_ =
        --Debug.log "scrollTop" scrollTop
        --_ =
        --Debug.log "height" height
        --_ =
        --Debug.log "patch" patch
        updateViewLine i viewLine =
            { viewLine
                | text =
                    lines
                        |> B.getLine i
                        |> Maybe.withDefault ""
                , tokens =
                    syntax
                        |> Array.get i
                        |> Maybe.withDefault []
            }

        ( ( by, bx ), ( ey, ex ) ) =
            patchRegion patch
    in
        case patch of
            Deletion _ _ ->
                let
                    --_ =
                    --Debug.log "insert" ( ( by, bx ), ( ey, ex ) )
                    maxLineNumber =
                        min (scrollTop + height) (B.count lines)

                    cnt =
                        ey - by + 1

                    n =
                        cnt - 1

                    --_ =
                    --    Debug.log "maxLineNumber" maxLineNumber
                    --_ =
                    --    Debug.log "cnt" cnt
                    --_ =
                    --    Debug.log "n" n
                    insertStart =
                        max scrollTop by

                    --|> Debug.log "insertStart"
                    inserts =
                        getViewLines
                            insertStart
                            (insertStart + cnt)
                            lines
                            syntax

                    --|> Debug.log "inserts"
                in
                    viewLines
                        |> List.map
                            (Maybe.andThen
                                (\({ lineNumber } as viewLine) ->
                                    --let
                                    --_ =
                                    --Debug.log "lineNumber" lineNumber
                                    --in
                                    if lineNumber == insertStart then
                                        Nothing
                                    else if lineNumber > insertStart then
                                        if lineNumber + n < maxLineNumber then
                                            Just
                                                { viewLine
                                                    | lineNumber =
                                                        lineNumber + n
                                                }
                                        else
                                            Nothing
                                    else
                                        Just viewLine
                                )
                            )
                        |> fillViewLines inserts

            Insertion _ _ ->
                let
                    --_ =
                    --Debug.log "delete" ( ( by, bx ), ( ey, ex ) )
                    maxLineNumber =
                        min (scrollTop + height) (B.count oldLines)

                    --_ =
                    --Debug.log "maxLineNumber" maxLineNumber
                    --_ =
                    --Debug.log "(by,ey)" ( by, ey )
                    dy =
                        ey - by

                    inserts =
                        getViewLines
                            (max3 (by + 1) (maxLineNumber - dy) scrollTop)
                            maxLineNumber
                            lines
                            syntax

                    --|> Debug.log "inserts"
                in
                    viewLines
                        |> List.map
                            (Maybe.andThen
                                (\({ lineNumber } as viewLine) ->
                                    if by == lineNumber then
                                        viewLine
                                            |> updateViewLine by
                                            |> Just
                                    else if
                                        (by < lineNumber)
                                            && (lineNumber <= ey)
                                    then
                                        Nothing
                                    else if lineNumber > ey then
                                        if lineNumber - dy < scrollTop then
                                            Nothing
                                        else
                                            Just
                                                { viewLine
                                                    | lineNumber =
                                                        lineNumber - dy
                                                }
                                    else
                                        Just viewLine
                                )
                            )
                        |> fillViewLines inserts


{-| batch edit text, keep cursor, save history
-}
transaction : List Patch -> Buffer -> Buffer
transaction patches buf =
    let
        scrollTop =
            finalScrollTop buf

        ( buf1, undoPatchs ) =
            List.foldl
                (\patch ( buf_, undoPatches_ ) ->
                    let
                        ( patch1, lines ) =
                            applyPatch patch buf_.lines

                        cursor =
                            updateCursor patch patch1 buf_.cursor

                        ( syntax, _ ) =
                            applyPatchToSyntax patch buf_.syntax

                        view =
                            buf_.view

                        --_ =
                        --    Debug.log "patch1" patch1
                        --_ =
                        --    Debug.log "buf.lines" buf.lines
                        --_ =
                        --    Debug.log "lines" lines
                        --_ =
                        --    Debug.log "view" view
                    in
                        ( { buf_
                            | lines = lines
                            , cursor = cursor
                            , syntax = syntax
                            , syntaxDirtyFrom =
                                min
                                    buf_.syntaxDirtyFrom
                                    (patch
                                        |> B.patchCursor
                                        |> Tuple.first
                                    )
                            , view =
                                { view
                                    | lines =
                                        applyPatchToViewLines
                                            scrollTop
                                            view.size.height
                                            patch1
                                            buf_.lines
                                            lines
                                            syntax
                                            view.lines

                                    --|> Debug.log "view.lines after"
                                }
                          }
                        , patch1 :: undoPatches_
                        )
                )
                ( buf, [] )
                patches

        --_ =
        --    Debug.log "patches" patches
        --_ =
        --    Debug.log "undoPatchs" undoPatchs
    in
        if List.isEmpty undoPatchs then
            buf
        else
            let
                history =
                    addPending buf.cursor undoPatchs buf1.history

                --_ =
                --    Debug.log "history" history
            in
                { buf1
                    | history =
                        { history
                            | version = history.version + 1
                            , pendingChanges = history.pendingChanges ++ patches
                        }
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
                    Deletion b e ->
                        let
                            ( by, bx ) =
                                b

                            ( ey, ex ) =
                                e

                            ( y, x ) =
                                cursor
                        in
                            ( y + (ey - by)
                            , if Tuple.first pos == y then
                                x + (ex - bx)
                              else
                                x
                            )

                    _ ->
                        cursor

        Deletion b e ->
            case patch1 of
                Insertion _ s ->
                    if isEmpty s then
                        cursor
                    else if cursor < b then
                        cursor
                    else if b <= cursor && cursor < e then
                        b
                    else
                        let
                            ( by, bx ) =
                                b

                            ( ey, ex ) =
                                e

                            ( y, x ) =
                                cursor
                        in
                            ( y - (ey - by)
                            , if ey == y then
                                x - (ex - bx)
                              else
                                x
                            )

                Deletion _ _ ->
                    cursor


addPending : Position -> List Patch -> BufferHistory -> BufferHistory
addPending cursor patches history =
    { history
        | pending =
            List.foldl
                (\patch result ->
                    case result.patches of
                        x :: xs ->
                            { result
                                | patches =
                                    case B.mergePatch patch x of
                                        Just patch1 ->
                                            patch1 :: xs

                                        _ ->
                                            patch :: x :: xs
                            }

                        _ ->
                            { patches = [ patch ]
                            , cursor = cursor
                            }
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

        { pending, undoes, pendingChanges } =
            history
    in
        case pending.patches of
            [] ->
                buf

            _ ->
                { buf
                    | history =
                        { history
                            | undoes = pending :: undoes
                            , pending = emptyUndo
                            , redoes = []
                            , savePoint = history.savePoint + 1
                            , pendingChanges = []
                            , changes = history.changes ++ pendingChanges
                        }
                }


{-| undo last change
-}
undo : Buffer -> Buffer
undo buf =
    buf.history.undoes
        |> List.head
        |> Maybe.map
            (\undo_ ->
                let
                    undoPatches =
                        undo_.patches

                    history =
                        buf.history

                    res =
                        applyPatches undoPatches buf

                    jumps =
                        applyPatchesToJumps undoPatches buf.jumps

                    lintErrors =
                        applyPatchesToLintErrors
                            buf.lint.items
                            undoPatches

                    undoHistory { undoes, redoes } =
                        let
                            savePoint =
                                history.savePoint - 1
                        in
                            { history
                                | undoes =
                                    List.tail undoes
                                        |> Maybe.withDefault []
                                , pending = emptyUndo
                                , redoes = { undo_ | patches = res.patches } :: redoes
                                , version = history.version + 1
                                , savePoint = savePoint
                                , changes =
                                    if savePoint == 0 then
                                        []
                                    else
                                        history.changes ++ undoPatches
                            }

                    view =
                        buf.view
                in
                    { buf
                        | lines = res.lines
                        , cursor = undo_.cursor
                        , cursorColumn = Tuple.second undo_.cursor
                        , syntax = res.syntax
                        , syntaxDirtyFrom =
                            undoPatches
                                |> List.map B.patchCursor
                                |> List.minimum
                                |> Maybe.map
                                    (Tuple.first >> min buf.syntaxDirtyFrom)
                                |> Maybe.withDefault buf.syntaxDirtyFrom
                        , history = undoHistory buf.history
                        , jumps = jumps
                        , lint =
                            { items = lintErrors
                            , count = buf.lint.count
                            }
                        , view = { view | lines = res.viewLines }
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
            (\redo_ ->
                let
                    redoPatches =
                        redo_.patches

                    history =
                        buf.history

                    res =
                        applyPatches redoPatches buf

                    jumps =
                        applyPatchesToJumps redoPatches buf.jumps

                    lintErrors =
                        applyPatchesToLintErrors buf.lint.items redoPatches

                    redoHistory { undoes, redoes } =
                        let
                            savePoint =
                                history.savePoint + 1
                        in
                            { history
                                | undoes = { redo_ | patches = res.patches } :: undoes
                                , pending = emptyUndo
                                , redoes =
                                    List.tail redoes
                                        |> Maybe.withDefault []
                                , version = history.version + 1
                                , savePoint = savePoint
                                , changes =
                                    if savePoint == 0 then
                                        []
                                    else
                                        history.changes ++ redoPatches
                            }

                    cursor =
                        res.patches
                            |> List.map B.patchCursor
                            |> List.minimum
                            |> Maybe.withDefault buf.cursor

                    view =
                        buf.view
                in
                    { buf
                        | lines = res.lines
                        , cursor = cursor
                        , cursorColumn = Tuple.second cursor
                        , syntax = res.syntax
                        , syntaxDirtyFrom =
                            redoPatches
                                |> List.map B.patchCursor
                                |> List.minimum
                                |> Maybe.map
                                    (Tuple.first >> min buf.syntaxDirtyFrom)
                                |> Maybe.withDefault buf.syntaxDirtyFrom
                        , history = redoHistory buf.history
                        , jumps = jumps
                        , lint =
                            { items = lintErrors
                            , count = buf.lint.count
                            }
                        , view = { view | lines = res.viewLines }
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
    List.head history.pending.patches


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


updateHistory : (BufferHistory -> BufferHistory) -> Buffer -> Buffer
updateHistory update buf =
    { buf | history = update buf.history }


putString : Bool -> RegisterText -> Buffer -> Buffer
putString forward text buf =
    let
        ( y, x ) =
            buf.cursor

        tabSize =
            buf.config.tabSize

        ( patch, cursor ) =
            let
                line =
                    getLine y buf.lines |> Maybe.withDefault ""
            in
                case text of
                    Model.Text s ->
                        if forward && line /= lineBreak then
                            ( s
                                |> fromStringExpandTabs tabSize (x + 1)
                                |> Insertion ( y, x + 1 )
                            , Nothing
                            )
                        else
                            ( s
                                |> fromStringExpandTabs tabSize x
                                |> Insertion ( y, x )
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
                                ( s1
                                    |> fromStringExpandTabs tabSize 0
                                    |> Insertion ( y + 1, 0 )
                                , Just ( y + 1, findLineFirst s + 1 )
                                )
                        else
                            case buf.mode of
                                Model.Insert _ ->
                                    ( s
                                        |> fromStringExpandTabs tabSize x
                                        |> Insertion ( y, x )
                                    , Nothing
                                    )

                                _ ->
                                    ( s
                                        |> fromStringExpandTabs tabSize 0
                                        |> Insertion ( y, 0 )
                                    , Just ( y, findLineFirst s + 1 )
                                    )

        getLastDeletedTo buf_ =
            case getLastPatch buf_ of
                Just patch_ ->
                    case patch_ of
                        Deletion _ to ->
                            to

                        _ ->
                            buf_.cursor

                _ ->
                    buf_.cursor
    in
        buf
            |> setLastIndent 0
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


cIndentRules :
    { decrease : Re.Regex
    , increase : Re.Regex
    , increaseNext : Re.Regex
    }
cIndentRules =
    { increase = regex "^.*\\{[^}\\\"']*$"
    , decrease = regex "^(.*\\*/)?\\s*\\}[;\\s]*$"
    , increaseNext =
        regex "^(?!.*;\\s*//).*[^\\s;{}]\\s*$"
    }


cssFileDefaultConfig : BufferConfig
cssFileDefaultConfig =
    { defaultBufferConfig
        | tabSize = 2
        , indent = IndentRules cIndentRules
        , wordChars = "_-.#"
    }


infoMessage : String -> Buffer -> Buffer
infoMessage s buf =
    case buf.mode of
        Normal data ->
            { buf
                | mode =
                    Normal { data | message = InfoMessage s }
            }

        Ex ex ->
            { buf
                | mode =
                    Ex { ex | message = InfoMessage s }
            }

        _ ->
            buf


errorMessage : String -> Buffer -> Buffer
errorMessage s buf =
    case buf.mode of
        Normal data ->
            { buf
                | mode =
                    Normal { data | message = ErrorMessage s }
            }

        Ex ex ->
            { buf
                | mode =
                    Ex { ex | message = ErrorMessage s }
            }

        _ ->
            buf


clearMessage : Buffer -> Buffer
clearMessage buf =
    case buf.mode of
        Normal data ->
            { buf
                | mode =
                    Normal { data | message = EmptyMessage }
            }

        Ex ex ->
            { buf
                | mode =
                    Ex { ex | message = EmptyMessage }
            }

        _ ->
            buf


configs : Dict String BufferConfig
configs =
    Dict.fromList
        [ ( ".elm"
          , { defaultBufferConfig
                | tabSize = 4
                , lint = True
                , indent =
                    IndentRules
                        { increase =
                            regex
                                ("(^[(]?let$)|(^[(]?if)"
                                    ++ "|(^then$)|(^else(\\s|$))|(=$)"
                                    ++ "|(^in$)|(^[(]?case)|(^of$)|(->$)"
                                )
                        , decrease = regex "^(then|else( if)?|of|in)"
                        , increaseNext = regex "![\\s\\S]"
                        }
            }
          )
        , ( ".js"
          , { defaultBufferConfig
                | tabSize = 2
                , lint = True
                , indent = IndentRules cIndentRules
            }
          )
        , ( ".jsx"
          , { defaultBufferConfig
                | tabSize = 2
                , lint = True
                , indent = IndentRules cIndentRules
            }
          )
        , ( ".purs"
          , { defaultBufferConfig
                | tabSize = 2
                , indent =
                    IndentRules
                        { increase =
                            regex
                                ("(^[(]?let$)|(^[(]?if)"
                                    ++ "|(^then$)|(^else(\\s|$))|(=$)"
                                    ++ "|(^in$)|(^[(]?case)|(^of$)|(->$)"
                                    ++ "|(^when)|(\\sdo$)"
                                )
                        , decrease = regex "^(then|else( if)?|of|in)"
                        , increaseNext = regex "![\\s\\S]"
                        }
            }
          )
        , ( ".less", cssFileDefaultConfig )
        , ( ".css", cssFileDefaultConfig )
        ]


setShowTip : Bool -> Buffer -> Buffer
setShowTip showTip buf =
    let
        view =
            buf.view
    in
        { buf | view = { view | showTip = showTip } }


isDirty : Buffer -> Bool
isDirty buf =
    buf.history.savePoint /= 0


isEditing : Buffer -> Buffer -> Bool
isEditing buf1 buf2 =
    buf1.history.version
        /= buf2.history.version


cursorLineFirst : B.TextBuffer -> Int -> Maybe Position
cursorLineFirst lines y =
    lines
        |> B.getLine y
        |> Maybe.map (findLineFirst >> (Tuple.pair y))


gotoLine : Int -> Buffer -> Buffer
gotoLine y buf =
    y
        |> cursorLineFirst buf.lines
        |> Maybe.map (\cursor -> setCursor cursor True buf)
        |> Maybe.withDefault buf


indentCursorToLineFirst : Buffer -> Buffer
indentCursorToLineFirst buf =
    let
        ( y, x ) =
            buf.cursor

        y1 =
            if y >= B.count buf.lines - 1 then
                Basics.max (y - 1) 0
            else
                y
    in
        if x == 0 then
            gotoLine y1 buf
        else
            buf


updateView : (View -> View) -> Buffer -> Buffer
updateView f buf =
    let
        view =
            buf.view
    in
        { buf | view = f buf.view }


{-| go through viewLines, test base on linenumber
if current line is removed or is nothing use inserts to fill this line
-}
fillViewLines :
    List (Maybe ViewLine)
    -> List (Maybe ViewLine)
    -> List (Maybe ViewLine)
fillViewLines inserts viewLines =
    case viewLines of
        viewLine :: restViewLines ->
            case viewLine of
                Just _ ->
                    viewLine :: fillViewLines inserts restViewLines

                _ ->
                    case inserts of
                        first :: restInserts ->
                            first
                                :: fillViewLines
                                    restInserts
                                    restViewLines

                        _ ->
                            viewLines

        _ ->
            []


fillEmptyViewLines : Int -> List (Maybe ViewLine) -> List (Maybe ViewLine)
fillEmptyViewLines height_ viewLines =
    let
        height =
            height_ + 2

        n =
            List.length viewLines
    in
        if n < height then
            viewLines ++ List.repeat (height - n) Nothing
        else
            viewLines


scrollViewLines :
    B.TextBuffer
    -> Syntax
    -> Int
    -> Int
    -> Int
    -> List (Maybe ViewLine)
    -> List (Maybe ViewLine)
scrollViewLines lines syntax height_ from to viewLines =
    let
        height =
            height_ + 2
    in
        if from == to then
            viewLines
        else if
            (to >= from + height)
                || (to + height <= from)
        then
            getViewLines
                to
                (to + height)
                lines
                syntax
        else
            let
                inserts =
                    if from < to then
                        getViewLines (from + height) (to + height) lines syntax
                    else
                        getViewLines to from lines syntax
            in
                viewLines
                    |> List.map
                        (Maybe.andThen
                            (\({ lineNumber } as viewLine) ->
                                if to <= lineNumber && lineNumber < to + height then
                                    Just viewLine
                                else
                                    Nothing
                            )
                        )
                    |> fillViewLines inserts


setScrollTop : Int -> Buffer -> Buffer
setScrollTop n buf =
    if n == buf.view.scrollTop then
        buf
    else
        updateView
            (\v ->
                { v
                    | scrollTop = n
                    , scrollTopPx =
                        if n == v.scrollTopPx // v.lineHeight then
                            v.scrollTopPx
                        else
                            n * buf.view.lineHeight
                    , lines =
                        scrollViewLines
                            buf.lines
                            buf.syntax
                            v.size.height
                            v.scrollTop
                            n
                            v.lines
                }
            )
            buf


bestScrollTop : Int -> Int -> B.TextBuffer -> Int -> Int
bestScrollTop y height lines scrollTop =
    let
        maxLine =
            B.count lines - 1
    in
        if scrollTop <= y && y < scrollTop + height then
            scrollTop
        else
            y
                - (height // 2)
                |> max 0


toWords : String -> Buffer -> List String
toWords exclude { config, lines, cursor } =
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


setLastIndent : Int -> Buffer -> Buffer
setLastIndent indent buf =
    let
        last =
            buf.last
    in
        { buf | last = { last | indent = indent } }


setCursorColumn : Int -> Buffer -> Buffer
setCursorColumn cursorColumn buf =
    { buf | cursorColumn = cursorColumn }


cancelLastIndent : Buffer -> Buffer
cancelLastIndent buf =
    if buf.last.indent > 0 then
        let
            ( y, _ ) =
                buf.cursor
        in
            buf
                |> transaction [ Deletion ( y, 0 ) ( y, buf.last.indent ) ]
                |> setLastIndent 0
                |> setCursorColumn buf.last.indent
    else
        buf


getStatusBar :
    Mode
    ->
        { text : String
        , cursor : Maybe Position
        , error : Bool
        }
getStatusBar mode =
    case mode of
        Normal { message } ->
            { text =
                case message of
                    InfoMessage s ->
                        s

                    ErrorMessage s ->
                        s

                    _ ->
                        "-- Normal --"
            , cursor = Nothing
            , error =
                case message of
                    ErrorMessage _ ->
                        True

                    _ ->
                        False
            }

        Visual { tipe } ->
            { text =
                case tipe of
                    VisualLine ->
                        "-- Visual Line --"

                    VisualBlock ->
                        "-- Visual Block --"

                    _ ->
                        "-- Visual --"
            , cursor = Nothing
            , error = False
            }

        Insert _ ->
            { text = "-- Insert --"
            , cursor = Nothing
            , error = False
            }

        TempNormal ->
            { text = "-- (Insert) --"
            , cursor = Nothing
            , error = False
            }

        Ex { exbuf } ->
            { text = B.toString exbuf.lines
            , cursor = Just exbuf.cursor
            , error = False
            }


finalScrollTop : Buffer -> Int
finalScrollTop buf =
    case buf.mode of
        Ex { prefix, visual } ->
            case prefix of
                ExSearch { match } ->
                    case match of
                        Just ( begin, end ) ->
                            bestScrollTop
                                (Basics.min
                                    (Tuple.first begin)
                                    (Tuple.first end)
                                )
                                buf.view.size.height
                                buf.lines
                                buf.view.scrollTop

                        _ ->
                            buf.view.scrollTop

                _ ->
                    buf.view.scrollTop

        _ ->
            buf.view.scrollTop


switchVisualEnd : Buffer -> Buffer
switchVisualEnd buf =
    case buf.mode of
        Visual { tipe, begin, end } ->
            buf
                |> setMode
                    (Visual
                        { tipe = tipe
                        , begin = end
                        , end = begin
                        }
                    )
                |> setCursor begin True

        _ ->
            buf


shortPath : Buffer -> String
shortPath buf =
    relativePath buf.config.pathSeperator buf.cwd buf.path
