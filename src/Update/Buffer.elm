module Update.Buffer exposing
    ( addBuffer
    , applyPatchesToLintErrors
    , bestScrollTop
    , bufferInfo
    , cancelLastIndent
    , clearHistory
    , clearMessage
    , commit
    , cursorLineFirst
    , delete
    , errorMessage
    , finalScrollTop
    , getStatusBar
    , gotoLine
    , indentCursorToLineFirst
    , infoMessage
    , insert
    , isDirty
    , isEditing
    , redo
    , removeBuffer
    , setRegister
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

import Array
import Dict exposing (Dict)
import Fs
import Helper.Helper exposing (..)
import Internal.Position exposing (..)
import Internal.PositionClass exposing (findLineFirst)
import Internal.Syntax exposing (Syntax, applyPatchToSyntax)
import Internal.TextBuffer as B
    exposing
        ( Patch(..)
        , RegionChange(..)
        , TextBuffer
        , applyPatch
        , fromString
        , getLine
        , isEmpty
        , patchToRegion
        , shiftPositionByRegionChange
        )
import Internal.Window as Win
import List
import Maybe
import Model exposing (..)
import Model.Buffer exposing (..)
import Model.BufferConfig exposing (..)
import Model.BufferHistory exposing (..)
import Model.Frame as Frame
import Model.Global exposing (..)
import Model.Lint exposing (..)
import Model.LoadBuffer exposing (..)
import Model.View exposing (..)
import String
import Vim.AST exposing (Direction(..), ModeName(..), MotionData(..), VisualType(..))


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


applyPatchesToLintErrors : String -> List LintError -> List RegionChange -> List LintError
applyPatchesToLintErrors path =
    List.foldl
        (\patch result ->
            List.filterMap
                (\lintError ->
                    if lintError.file == path then
                        applyPatchToLintError patch lintError

                    else
                        Just lintError
                )
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
        Insertion pos _ ->
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


setRegister :
    String
    -> RegisterText
    -> { a | registers : Dict String RegisterText }
    -> { a | registers : Dict String RegisterText }
setRegister reg val global =
    { global | registers = Dict.insert reg val global.registers }


updateHistory : (BufferHistory -> BufferHistory) -> Buffer -> Buffer
updateHistory update buf =
    { buf | history = update buf.history }


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


setShowTip : Bool -> Global -> Global
setShowTip showTip global =
    { global | showTip = showTip }


isDirty : Buffer -> Bool
isDirty buf =
    buf.history.savePoint /= 0


isEditing : Buffer -> Buffer -> Bool
isEditing buf1 buf2 =
    buf1.history.version /= buf2.history.version


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
    { buf | view = f buf.view }


bestScrollTop : Int -> Int -> Int -> Int
bestScrollTop y height scrollTop =
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


cancelLastIndent : Buffer -> Buffer
cancelLastIndent buf =
    if buf.dirtyIndent > 0 then
        let
            ( y, _ ) =
                buf.view.cursor
        in
        { buf | dirtyIndent = 0 }
            |> transaction [ Deletion ( y, 0 ) ( y, buf.dirtyIndent ) ]
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


finalScrollTop : Buffer -> Int
finalScrollTop ({ view } as buf) =
    if view.isActive then
        case buf.mode of
            Ex { prefix } ->
                case prefix of
                    ExSearch { match } ->
                        case match of
                            Just ( begin, end ) ->
                                bestScrollTop
                                    (Basics.min
                                        (Tuple.first begin)
                                        (Tuple.first end)
                                    )
                                    view.size.height
                                    view.scrollTop

                            _ ->
                                view.scrollTop

                    _ ->
                        view.scrollTop

            _ ->
                view.scrollTop

    else
        view.scrollTop


switchVisualEnd : Buffer -> Buffer
switchVisualEnd buf =
    case buf.mode of
        Visual { tipe, begin, end } ->
            { buf
                | mode =
                    Visual
                        { tipe = tipe
                        , begin = end
                        , end = begin
                        }
            }
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
        Fs.shortPath global.fs path


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


bufferInfo : Global -> Buffer -> String
bufferInfo global buf =
    let
        n =
            B.count buf.lines - 1

        ( y, x ) =
            buf.view.cursor
    in
    (buf |> shortBufferPath global |> quote)
        ++ " line "
        ++ (y
                |> inc
                |> String.fromInt
           )
        ++ " of "
        ++ (B.count buf.lines |> String.fromInt)
        ++ " --"
        ++ (y * 100 // n |> String.fromInt)
        ++ "%-- col "
        ++ (x
                |> inc
                |> String.fromInt
           )
