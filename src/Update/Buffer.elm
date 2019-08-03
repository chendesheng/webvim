module Update.Buffer exposing
    ( addBuffer
    , applyDiffToView
    , applyPatchesToLintErrors
    , applyRegionChangeToView
    , bestScrollTop
    , cIndentRules
    , cancelLastIndent
    , clearHistory
    , clearMessage
    , commit
    , configs
    , cursorLineFirst
    , delete
    , disableSyntax
    , errorMessage
    , finalScrollTop
    , getLastDeleted
    , getStatusBar
    , gotoLine
    , indentCursorToLineFirst
    , infoMessage
    , insert
    , isDirty
    , isEditing
    , putString
    , redo
    , removeBuffer
    , scrollViewLines
    , setCursor
    , setCursorColumn
    , setLastIndent
    , setMode
    , setRegister
    , setScrollTop
    , setShowTip
    , shortBufferPath
    , shortPath
    , switchVisualEnd
    , toWords
    , transaction
    , undo
    , updateHistory
    , updateView
    )

import Array as Array exposing (Array)
import Dict exposing (Dict)
import Helper.Helper
    exposing
        ( filename
        , findFirst
        , parseWords
        , rangeCount
        , regex
        , relativePath
        )
import Internal.Position exposing (..)
import Internal.PositionClass exposing (findLineFirst)
import Internal.Syntax
    exposing
        ( Syntax
        , Token
        , applyPatchToSyntax
        , splitTokens
        )
import Internal.TextBuffer as B
    exposing
        ( Patch(..)
        , RegionChange(..)
        , TextBuffer
        , applyPatch
        , foldlLines
        , fromString
        , fromStringExpandTabs
        , getLine
        , isEmpty
        , lineBreak
        , patchToRegion
        , shiftPositionByRegionChange
        )
import Internal.Window as Win exposing (Window)
import List
import Maybe
import Model
    exposing
        ( Buffer
        , BufferConfig
        , BufferHistory
        , ExPrefix(..)
        , Global
        , IndentConfig(..)
        , LintError
        , LoadBuffer(..)
        , Mode(..)
        , RegisterText
        , StatusMessage(..)
        , defaultBufferConfig
        , emptyBuffer
        , emptyBufferHistory
        , emptyUndo
        , getBuffer
        , getLoadedBuffer
        , isTempBuffer
        )
import Model.Frame as Frame
import Model.Size exposing (Size)
import Model.View exposing (View, emptyView)
import Regex as Re
import String
import Vim.AST
    exposing
        ( Direction(..)
        , ModeName(..)
        , MotionData(..)
        , MotionOption
        , VisualType(..)
        )


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
applyInsertionToView from size scrollTop scrollBottom viewLines =
    if size == 0 || from >= scrollBottom then
        viewLines

    else
        applyInsertionToViewHelper from
            size
            scrollBottom
            (max from scrollTop)
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
applyInsertionToViewHelper from size scrollBottom i viewLines =
    -- go through each line
    -- if n < from, keep it not change
    -- if n >= from, increase size
    -- after increase size check if it's >= scrollBottom
    -- if it is, replace it to inserting line
    case viewLines of
        n :: rest ->
            if n < from then
                n :: applyInsertionToViewHelper from size scrollBottom i rest

            else if n + size < scrollBottom then
                (n + size) :: applyInsertionToViewHelper from size scrollBottom i rest

            else
                i :: applyInsertionToViewHelper from size scrollBottom (i + 1) rest

        _ ->
            []


applyDeletionToView : Int -> Int -> Int -> List Int -> List Int
applyDeletionToView from to scrollBottom viewLines =
    let
        size =
            to - from
    in
    if size <= 0 || from >= scrollBottom then
        viewLines

    else
        applyDeletionToViewHelper from
            to
            size
            (max from (scrollBottom - size))
            viewLines


applyDeletionToViewHelper : Int -> Int -> Int -> Int -> List Int -> List Int
applyDeletionToViewHelper from to size i viewLines =
    case viewLines of
        n :: rest ->
            if n < from then
                n :: applyDeletionToViewHelper from to size i rest

            else if n < to then
                i :: applyDeletionToViewHelper from to size (i + 1) rest

            else
                (n - size) :: applyDeletionToViewHelper from to size i rest

        _ ->
            []


applyDiffToView : List RegionChange -> Int -> Int -> List Int -> List Int
applyDiffToView diff scrollTop height viewLines =
    List.foldl
        (\change viewLines1 ->
            applyRegionChangeToView change scrollTop (height + 2) viewLines1
        )
        viewLines
        diff


applyRegionChangeToView : RegionChange -> Int -> Int -> List Int -> List Int
applyRegionChangeToView change scrollTop height viewLines =
    let
        scrollBottom =
            scrollTop + height
    in
    case change of
        RegionAdd ( ( by, _ ), ( ey, _ ) ) ->
            applyInsertionToView by (ey - by) scrollTop scrollBottom viewLines

        RegionRemove ( ( by, _ ), ( ey, _ ) ) ->
            applyDeletionToView by ey scrollBottom viewLines


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
                    , pendingChanges =
                        mergePatches history.pendingChanges patches
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


mergePatches : List Patch -> List Patch -> List Patch
mergePatches patches target =
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
        target
        (List.reverse patches)


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


setCursor : Position -> Bool -> View -> View
setCursor cursor saveColumn view =
    { view
        | cursor = cursor
        , cursorColumn =
            if saveColumn then
                Tuple.second cursor

            else
                view.cursorColumn
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
                updateView
                    (setCursor
                        (case cursor of
                            Just p ->
                                p

                            Nothing ->
                                getLastDeletedTo buf1
                        )
                        True
                    )
                    buf1
           )


cIndentRules :
    { decrease : Re.Regex
    , increase : Re.Regex
    , increaseNext : Re.Regex
    , trigger : String
    }
cIndentRules =
    { increase = regex "^.*\\{[^}\\\"']*$"
    , decrease = regex "^(.*\\*/)?\\s*\\}[;\\s]*$"
    , increaseNext =
        regex "^(?!.*;\\s*//).*[^\\s;{}]\\s*$"
    , trigger = "}"
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
                        , trigger = ""
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
                        , trigger = ""
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
        |> Maybe.map (findLineFirst >> Tuple.pair y)


gotoLine : Int -> Buffer -> Buffer
gotoLine y buf =
    y
        |> cursorLineFirst buf.lines
        |> Maybe.map (\cursor -> updateView (setCursor cursor True) buf)
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


scrollViewLinesHelper : Int -> Int -> Int -> Int -> List Int -> List Int
scrollViewLinesHelper height from to i viewLines =
    -- height MUST equal to List.length viewLines
    -- from <= n < from + height
    case viewLines of
        n :: rest ->
            if to <= n && n < to + height then
                n :: scrollViewLinesHelper height from to i rest

            else
                -- n < to || to + height <= n
                i :: scrollViewLinesHelper height from to (i + 1) rest

        _ ->
            []


scrollViewLines : Int -> Int -> Int -> List Int -> List Int
scrollViewLines height_ from to viewLines =
    let
        height =
            height_ + 2
    in
    if from == to then
        viewLines

    else if from + height <= to || to + height <= from then
        rangeCount to height

    else if from > to then
        -- show up contents
        scrollViewLinesHelper height from to to viewLines

    else if from < to then
        -- show down contents
        -- Prove of i < to + height:
        --   n is item in viewLines, this means from <= n < from + height
        --   also when to <= n < to + height, n will preserve
        --   thus when to <= n < from + height, n will preserve
        --   thus (from + height - to) items in viewLines will preserve
        --   thus i will increase height - (from + height - to) times
        --   since i start from (from + height),
        --     i < (from + height) + height - (from + height - to) = to + height
        scrollViewLinesHelper height from to (from + height) viewLines

    else
        viewLines


setScrollTop : Int -> Int -> View -> View
setScrollTop n lineHeight view =
    if n == view.scrollTop then
        view

    else
        let
            viewLines =
                scrollViewLines view.size.height view.scrollTop n view.lines
        in
        { view
            | scrollTop = n
            , scrollTopPx =
                if n == view.scrollTopPx // lineHeight then
                    view.scrollTopPx

                else
                    n * lineHeight
            , lines = viewLines
            , gutterLines =
                if view.scrollTop == n then
                    view.gutterLines

                else
                    viewLines
        }


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


setCursorColumn : Int -> View -> View
setCursorColumn cursorColumn view =
    { view | cursorColumn = cursorColumn }


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
            |> updateView (setCursorColumn buf.dirtyIndent)

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


finalScrollTop : Size -> View -> Buffer -> Int
finalScrollTop { height } view buf =
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
                                view.scrollTop

                        _ ->
                            view.scrollTop

                _ ->
                    view.scrollTop

        _ ->
            view.scrollTop


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
                |> updateView (setCursor begin True)

        _ ->
            buf


shortBufferPath : Global -> Buffer -> String
shortBufferPath global buf =
    shortPath global buf.path


shortPath : Global -> String -> String
shortPath global path =
    if isTempBuffer path then
        path

    else
        relativePath global.pathSeperator global.cwd path


activeBuffer : String -> Global -> Global
activeBuffer id global =
    if
        -- same buffer
        global.window
            |> Win.getActiveFrame
            |> Maybe.andThen Frame.getActiveViewId
            |> Maybe.map ((==) id)
            |> Maybe.withDefault False
    then
        global

    else
        case getBuffer id global.buffers of
            Just { view } ->
                { global
                    | window =
                        Win.updateActiveFrame
                            (Frame.updateActiveView <| always view)
                            global.window
                }

            _ ->
                global


addBuffer : Buffer -> Global -> Global
addBuffer buf global =
    { global
        | buffers = Dict.insert buf.id (Loaded buf) global.buffers
    }


removeBuffer : String -> Global -> Global
removeBuffer id global =
    { global
        | buffers =
            Dict.remove id global.buffers
    }


disableSyntax : BufferConfig -> BufferConfig
disableSyntax config =
    { config | syntax = False }
