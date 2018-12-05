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
        , scrollViewLines
        , switchVisualEnd
        , shortPath
        , updateHistory
        , applyPatchesToLintErrors
        , applyDiffToView
        , addBuffer
        , resizeView
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
        , IME
        , emptyIme
        , Global
        , Size
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
        , shiftPositionByRegionChange
        , patchToRegion
        , RegionChange(..)
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
import Helper.Helper exposing (parseWords, relativePath, regex, filename)
import Regex as Re
import Internal.Window as Win exposing (Window)


reversedPatchToRegionChange : Patch -> RegionChange
reversedPatchToRegionChange patch =
    case patch of
        Insertion _ _ ->
            RegionRemove (patchToRegion patch)

        _ ->
            RegionAdd (patchToRegion patch)


applyPatchToLintError : RegionChange -> LintError -> Maybe LintError
applyPatchToLintError patch ({ region, subRegion } as error) =
    let
        updateRegion ( b, e ) =
            case patch of
                RegionRemove ( bDel, eDel ) ->
                    if bDel <= b && e <= eDel then
                        Nothing
                    else
                        Just
                            ( shiftPositionByRegionChange patch b
                            , shiftPositionByRegionChange patch e
                            )

                RegionAdd _ ->
                    Just
                        ( shiftPositionByRegionChange patch b
                        , shiftPositionByRegionChange patch e
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


applyPatchesToLintErrors : List LintError -> List RegionChange -> List LintError
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
        }
applyPatches patches buf =
    let
        lines =
            buf.lines
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
                    }
            )
            { lines = lines
            , patches = []
            , syntax = buf.syntax
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


applyInsertionToView : Int -> Int -> Int -> Int -> List Int -> List Int
applyInsertionToView from size scrollTop height viewLines =
    if size == 0 then
        viewLines
    else
        applyInsertionToViewHelper from
            (max from scrollTop)
            size
            (scrollTop + height)
            viewLines


{-| start at [ 0 , 1 , 2 ]

Ins 0 1
[ 1 , 2 , 0 ]

Ins 1 2
[ 1 , 2 , 0 ]

Ins 0 1
[ 2 , 0 , 1 ]

Ins 2 1
[ 2 , 0 , 1 ]

-}
applyInsertionToViewHelper : Int -> Int -> Int -> Int -> List Int -> List Int
applyInsertionToViewHelper from i size scrollBottom viewLines =
    -- go through each line
    -- if n < from, keep it not change
    -- if n >= from, increase size
    -- after increase size check if it's >= scrollBottom
    -- if it is, replace it to inserting line
    case viewLines of
        n :: rest ->
            let
                n1 =
                    if n < from then
                        n
                    else
                        n
                            + size

                replace =
                    n1 >= scrollBottom
            in
                (if replace then
                    i
                 else
                    n1
                )
                    :: applyInsertionToViewHelper from
                        (if replace then
                            i + 1
                         else
                            i
                        )
                        size
                        scrollBottom
                        rest

        _ ->
            []


applyDeletionToView : Int -> Int -> Int -> Int -> List Int -> List Int
applyDeletionToView from to scrollTop height viewLines =
    let
        size =
            to - from
    in
        if size <= 0 then
            viewLines
        else
            applyDeletionToViewHelper from
                to
                size
                (max from (scrollTop + height - size))
                viewLines


applyDeletionToViewHelper : Int -> Int -> Int -> Int -> List Int -> List Int
applyDeletionToViewHelper from to size i viewLines =
    case viewLines of
        n :: rest ->
            (if n < from then
                n
             else if n >= to then
                n - size
             else
                i
            )
                :: applyDeletionToViewHelper from
                    to
                    size
                    (if from <= n && n < to then
                        i + 1
                     else
                        i
                    )
                    rest

        _ ->
            []


applyDiffToView : List RegionChange -> Int -> Int -> List Int -> List Int
applyDiffToView diff scrollTop height viewLines =
    List.foldl
        (\change viewLines1 ->
            applyRegionChangeToView change scrollTop height viewLines1
        )
        viewLines
        diff


applyRegionChangeToView : RegionChange -> Int -> Int -> List Int -> List Int
applyRegionChangeToView change scrollTop height_ viewLines =
    let
        height =
            height_ + 2
    in
        case change of
            RegionAdd ( ( by, bx ), ( ey, ex ) ) ->
                applyInsertionToView by (ey - by) scrollTop height viewLines

            RegionRemove ( ( by, bx ), ( ey, ex ) ) ->
                applyDeletionToView
                    (if bx == 0 then
                        by
                     else
                        by + 1
                    )
                    ey
                    scrollTop
                    height
                    viewLines


{-| batch edit text, keep cursor, save history
-}
transaction : List Patch -> Buffer -> Buffer
transaction patches buf =
    let
        ( buf1, undoPatchs ) =
            List.foldl
                (\patch ( buf_, undoPatches_ ) ->
                    let
                        ( patch1, lines ) =
                            applyPatch patch buf_.lines

                        cursor =
                            updateCursor patch patch1 buf_.view.cursor

                        ( syntax, _ ) =
                            applyPatchToSyntax patch buf_.syntax

                        --_ =
                        --    Debug.log "patch1" patch1
                        --_ =
                        --    Debug.log "buf.lines" buf.lines
                        --_ =
                        --    Debug.log "lines" lines
                        --_ =
                        --    Debug.log "view" view
                        view =
                            buf.view
                    in
                        ( { buf_
                            | lines = lines
                            , view = { view | cursor = cursor }
                            , syntax = syntax
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
                    addPending buf.view.cursor undoPatchs buf1.history

                --_ =
                --    Debug.log "history" history
            in
                { buf1
                    | history =
                        { history
                            | version = history.version + 1
                            , pendingChanges = history.pendingChanges ++ patches
                            , diff =
                                List.map reversedPatchToRegionChange undoPatchs
                                    ++ history.diff
                        }
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
                                , diff =
                                    List.map reversedPatchToRegionChange res.patches
                                        ++ history.diff
                            }

                    view =
                        buf.view
                in
                    { buf
                        | lines = res.lines
                        , view =
                            { view
                                | cursor = undo_.cursor
                                , cursorColumn = Tuple.second undo_.cursor
                            }
                        , syntax = res.syntax
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
            (\redo_ ->
                let
                    redoPatches =
                        redo_.patches

                    history =
                        buf.history

                    res =
                        applyPatches redoPatches buf

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
                                , diff =
                                    List.map reversedPatchToRegionChange res.patches
                                        ++ history.diff
                            }

                    cursor =
                        res.patches
                            |> List.map B.patchCursor
                            |> List.minimum
                            |> Maybe.withDefault buf.view.cursor

                    view =
                        buf.view
                in
                    { buf
                        | lines = res.lines
                        , view =
                            { view
                                | cursor = cursor
                                , cursorColumn = Tuple.second cursor
                            }
                        , syntax = res.syntax
                        , history = redoHistory buf.history
                    }
            )
        |> Maybe.withDefault buf


moveByClass : MotionData -> MotionOption -> Buffer -> Buffer
moveByClass class option buf =
    let
        ( y, x ) =
            buf.view.cursor
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


setRegister :
    String
    -> RegisterText
    -> { a | registers : Dict String RegisterText }
    -> { a | registers : Dict String RegisterText }
setRegister reg val global =
    { global | registers = Dict.insert reg val global.registers }


setMode : Mode -> Buffer -> Buffer
setMode mode buf =
    { buf | mode = mode }


setCursor : Position -> Bool -> Buffer -> Buffer
setCursor cursor saveColumn buf =
    let
        view =
            buf.view
    in
        { buf
            | view =
                { view
                    | cursor = cursor
                    , cursorColumn =
                        if saveColumn then
                            Tuple.second cursor
                        else
                            view.cursorColumn
                }
        }


updateHistory : (BufferHistory -> BufferHistory) -> Buffer -> Buffer
updateHistory update buf =
    { buf | history = update buf.history }


putString : Bool -> RegisterText -> Buffer -> Buffer
putString forward text buf =
    let
        ( y, x ) =
            buf.view.cursor

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
                            buf_.view.cursor

                _ ->
                    buf_.view.cursor
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


setShowTip : Bool -> Global -> Global
setShowTip showTip global =
    { global | showTip = showTip }


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
            buf.view.cursor

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


scrollViewLinesHelper :
    Int
    -> Int
    -> Int
    -> Int
    -> List Int
    -> List Int
scrollViewLinesHelper height from to i viewLines =
    case viewLines of
        n :: rest ->
            let
                replace =
                    n < to || n >= to + height
            in
                (if replace then
                    i
                 else
                    n
                )
                    :: scrollViewLinesHelper
                        height
                        from
                        to
                        (if replace then
                            i + 1
                         else
                            i
                        )
                        rest

        _ ->
            []


scrollViewLines :
    Int
    -> Int
    -> Int
    -> List Int
    -> List Int
scrollViewLines height_ from to viewLines =
    let
        height =
            height_ + 2
    in
        if from + height <= to || to + height <= from then
            List.range to (to + height - 1)
        else if from > to then
            -- show up contents
            scrollViewLinesHelper
                height
                from
                to
                to
                viewLines
        else if from < to then
            -- show down contents
            scrollViewLinesHelper
                height
                from
                to
                (from + height)
                viewLines
        else
            viewLines


setScrollTop : Int -> Global -> Buffer -> Buffer
setScrollTop n global buf =
    if n == buf.view.scrollTop then
        buf
    else
        let
            lineHeight =
                global.lineHeight

            height =
                buf.view.size.height
        in
            updateView
                (\v ->
                    { v
                        | scrollTop = n
                        , scrollTopPx =
                            if n == v.scrollTopPx // lineHeight then
                                v.scrollTopPx
                            else
                                n * lineHeight
                        , lines =
                            scrollViewLines
                                height
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
    { buf | dirtyIndent = indent }


setCursorColumn : Int -> Buffer -> Buffer
setCursorColumn cursorColumn ({ view } as buf) =
    { buf | view = { view | cursorColumn = cursorColumn } }


cancelLastIndent : Buffer -> Buffer
cancelLastIndent buf =
    if buf.dirtyIndent > 0 then
        let
            ( y, _ ) =
                buf.view.cursor
        in
            buf
                |> transaction [ Deletion ( y, 0 ) ( y, buf.dirtyIndent ) ]
                |> setLastIndent 0
                |> setCursorColumn buf.dirtyIndent
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
            , cursor = Just exbuf.view.cursor
            , error = False
            }


finalScrollTop : Size -> Buffer -> Int
finalScrollTop { height } buf =
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
                                height
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


shortPath : Global -> Buffer -> String
shortPath global buf =
    relativePath global.pathSeperator global.cwd buf.path


activeBuffer : Int -> Global -> Global
activeBuffer id global =
    if
        -- same buffer
        (case Win.getActiveView global.window of
            Just view ->
                view.bufId == id

            _ ->
                False
        )
    then
        global
    else
        case
            global.buffers
                |> Dict.get id
                |> Maybe.map .view
        of
            Just view ->
                { global | window = Win.updateView (always view) global.window }

            _ ->
                global


addBuffer : Bool -> Buffer -> Global -> Global
addBuffer setActive buf global =
    let
        global1 =
            { global
                | buffers = Dict.insert buf.id buf global.buffers
            }
    in
        if setActive then
            activeBuffer buf.id global1
        else
            global1


resizeView : Size -> View -> View
resizeView size view =
    if size == view.size then
        view
    else
        { view
            | size = size
            , lines =
                List.range view.scrollTop
                    (view.scrollTop + size.height + 1)
        }
