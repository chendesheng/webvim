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
import Elm.Array as Array exposing (Array)
import Internal.Jumps exposing (applyPatchesToJumps, applyPatchesToLocations)
import Helper.Helper exposing (parseWords, relativePath)
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
    -> ( TextBuffer, List Patch, Int, Syntax, List (Maybe ViewLine) )
applyPatches patches buf =
    let
        lines =
            buf.lines

        view =
            buf.view
    in
        List.foldl
            (\patch ( lines, patches2, miny, syntax, viewLines ) ->
                let
                    ( patch2, lines2 ) =
                        applyPatch patch lines

                    syntax2 =
                        Tuple.first <| applyPatchToSyntax patch syntax
                in
                    ( lines2
                    , patch2 :: patches2
                    , min miny (minLine patch)
                    , syntax2
                    , applyPatchToViewLines
                        view.scrollTop
                        view.size.height
                        patch2
                        lines
                        lines2
                        syntax2
                        viewLines
                    )
            )
            ( lines, [], 0xFFFFFFFF, buf.syntax, view.lines )
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

                    --_ =
                    --Debug.log "maxLineNumber" maxLineNumber
                    cnt =
                        ey - by + 1

                    n =
                        cnt - 1

                    --|> Debug.log "n"
                    --_ =
                    --Debug.log "y" by
                    inserts =
                        getViewLines
                            (by + 1)
                            (by + n + 1)
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
                                    if lineNumber == by then
                                        viewLine
                                            |> updateViewLine by
                                            |> Just
                                    else if lineNumber > by then
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

                        view =
                            buf.view
                    in
                        ( { buf
                            | lines = lines
                            , cursor = cursor
                            , syntax = syntax
                            , syntaxDirtyFrom =
                                min
                                    buf.syntaxDirtyFrom
                                    (patch
                                        |> B.patchCursor
                                        |> Tuple.first
                                    )
                            , view =
                                { view
                                    | lines =
                                        applyPatchToViewLines
                                            view.scrollTop
                                            view.size.height
                                            patch1
                                            buf.lines
                                            lines
                                            syntax
                                            view.lines
                                }
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
                    addPending buf.cursor undo buf1.history

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

        { pending, undoes } =
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
                    undoPatches =
                        undo.patches

                    history =
                        buf.history

                    ( lines1, patches1, miny, syntax, viewLines ) =
                        applyPatches undoPatches buf

                    jumps =
                        applyPatchesToJumps undoPatches buf.jumps

                    lintErrors =
                        applyPatchesToLintErrors
                            buf.lint.items
                            undoPatches

                    undoHistory { undoes, redoes } =
                        { history
                            | undoes =
                                List.tail undoes
                                    |> Maybe.withDefault []
                            , pending = emptyUndo
                            , redoes = { undo | patches = patches1 } :: redoes
                            , version = history.version + 1
                            , savePoint = history.savePoint - 1
                        }

                    view =
                        buf.view
                in
                    { buf
                        | lines = lines1
                        , cursor = undo.cursor
                        , cursorColumn = Tuple.second undo.cursor
                        , syntax = syntax
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
                        , view = { view | lines = viewLines }
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
                    redoPatches =
                        redo.patches

                    history =
                        buf.history

                    ( lines1, patches1, miny, syntax, viewLines ) =
                        applyPatches redoPatches buf

                    jumps =
                        applyPatchesToJumps redoPatches buf.jumps

                    lintErrors =
                        applyPatchesToLintErrors buf.lint.items redoPatches

                    redoHistory { undoes, redoes } =
                        { history
                            | undoes = { redo | patches = patches1 } :: undoes
                            , pending = emptyUndo
                            , redoes =
                                List.tail redoes
                                    |> Maybe.withDefault []
                            , version = history.version + 1
                            , savePoint = history.savePoint + 1
                        }

                    cursor =
                        patches1
                            |> List.map B.patchCursor
                            |> List.minimum
                            |> Maybe.withDefault buf.cursor

                    view =
                        buf.view
                in
                    { buf
                        | lines = lines1
                        , cursor = cursor
                        , cursorColumn = Tuple.second cursor
                        , syntax = syntax
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
                        , view = { view | lines = viewLines }
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
    { increase = Re.regex "^.*\\{[^}\\\"']*$"
    , decrease = Re.regex "^(.*\\*/)?\\s*\\}[;\\s]*$"
    , increaseNext =
        Re.regex "^(?!.*;\\s*//).*[^\\s;{}]\\s*$"
    }


cssFileDefaultConfig : BufferConfig
cssFileDefaultConfig =
    { defaultBufferConfig
        | tabSize = 2
        , indent = IndentRules cIndentRules
        , wordChars = "_-"
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
                            Re.regex
                                ("(^[(]?let$)|(^[(]?if)"
                                    ++ "|(^then$)|(^else(\\s|$))|(=$)"
                                    ++ "|(^in$)|(^[(]?case)|(^of$)|(->$)"
                                )
                        , decrease = Re.regex "^(then|else( if)?|of|in)"
                        , increaseNext = Re.regex "![\\s\\S]"
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
                            Re.regex
                                ("(^[(]?let$)|(^[(]?if)"
                                    ++ "|(^then$)|(^else(\\s|$))|(=$)"
                                    ++ "|(^in$)|(^[(]?case)|(^of$)|(->$)"
                                    ++ "|(^when)|(\\sdo$)"
                                )
                        , decrease = Re.regex "^(then|else( if)?|of|in)"
                        , increaseNext = Re.regex "![\\s\\S]"
                        }
            }
          )
        , ( ".less", cssFileDefaultConfig )
        , ( ".css", cssFileDefaultConfig )
        ]


updateSavePoint : Buffer -> Buffer
updateSavePoint buf =
    let
        history =
            buf.history
    in
        { buf
            | history = { history | savePoint = 0 }
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
    buf.history.savePoint /= 0


isEditing : Buffer -> Buffer -> Bool
isEditing buf1 buf2 =
    buf1.history.version
        /= buf2.history.version


cursorLineFirst : B.TextBuffer -> Int -> Maybe Position
cursorLineFirst lines y =
    lines
        |> B.getLine y
        |> Maybe.map (findLineFirst >> ((,) y))


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
    if List.isEmpty inserts then
        viewLines
    else
        let
            fillInserts viewLines inserts =
                case inserts of
                    viewLine :: restInserts ->
                        viewLine
                            :: fillViewLines
                                restInserts
                                viewLines

                    _ ->
                        Nothing
                            :: fillViewLines
                                []
                                viewLines
        in
            case viewLines of
                viewLine :: rest ->
                    case viewLine of
                        Just { lineNumber } ->
                            viewLine :: fillViewLines inserts rest

                        _ ->
                            fillInserts rest inserts

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
