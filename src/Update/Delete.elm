module Update.Delete
    exposing
        ( delete
        , join
        , toRegisterText
        )

import Update.Range exposing (operatorRanges, isLinewise)
import Model exposing (..)
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Update.Buffer as Buf
import Update.Motion exposing (..)
import Internal.PositionClass exposing (findLineFirst)
import Helper.Helper exposing (getLast)


deleteOperator : Maybe Int -> V.OperatorRange -> Global -> Buffer -> List Patch
deleteOperator count range global buf =
    let
        ranges =
            operatorRanges count range global buf

        --Ex { visual } ->
        --    case visual of
        --        Just v ->
        --            let
        --                { begin, end } =
        --                    v
        --                begin1 =
        --                    min begin end
        --            in
        --                Just ( begin1, True )
        --        _ ->
        --            Nothing
    in
        if List.isEmpty ranges then
            []
        else
            List.map (\( begin, end ) -> Deletion begin end) ranges


delete : Maybe Int -> String -> V.OperatorRange -> Editor -> Editor
delete count register rg ({ global, buf } as ed) =
    let
        updateCursorColumn : Buffer -> Buffer
        updateCursorColumn buf_ =
            let
                view =
                    buf_.view
            in
                { buf_ | view = { view | cursorColumn = Tuple.second buf_.view.cursor } }

        linewise =
            isLinewise rg buf.mode

        doDelete : Buffer -> Buffer
        doDelete buf_ =
            case deleteOperator count rg global buf_ of
                [] ->
                    { buf_ | motionFailed = True }

                patches ->
                    let
                        setCursor : Buffer -> Buffer
                        setCursor buf__ =
                            case getLast patches of
                                Just (Deletion b e) ->
                                    Buf.updateView (Buf.setCursor b True) buf__

                                _ ->
                                    buf__
                    in
                        buf_
                            |> Buf.transaction patches
                            |> setCursor
    in
        case buf.mode of
            Ex ({ exbuf } as ex) ->
                -- FIXME: too hack!!
                case rg of
                    V.MotionRange md mo ->
                        case md of
                            V.MatchString _ ->
                                let
                                    buf1 =
                                        doDelete buf
                                in
                                    if buf1.motionFailed then
                                        { ed
                                            | global = saveMotion md mo buf buf1 global
                                            , buf = buf1
                                        }
                                    else
                                        { ed
                                            | global =
                                                global
                                                    |> saveLastDeleted linewise register buf1
                                                    |> saveMotion md mo buf buf1
                                            , buf = updateCursorColumn buf1
                                        }

                            _ ->
                                { ed
                                    | buf =
                                        Buf.setMode
                                            (Ex { ex | exbuf = doDelete exbuf })
                                            buf
                                }

                    _ ->
                        { ed
                            | buf =
                                Buf.setMode
                                    (Ex { ex | exbuf = doDelete exbuf })
                                    buf
                        }

            Insert _ ->
                { ed
                    | buf =
                        buf
                            |> doDelete
                            |> Buf.setLastIndent 0
                }

            _ ->
                let
                    buf1 =
                        doDelete buf
                in
                    if buf1.motionFailed then
                        { ed | buf = buf1 }
                    else
                        { ed
                            | global =
                                saveLastDeleted linewise register buf1 global
                            , buf =
                                buf1
                                    |> indentCursor linewise
                                    |> updateCursorColumn
                        }


indentCursor : Bool -> Buffer -> Buffer
indentCursor linewise buf =
    if linewise then
        Buf.indentCursorToLineFirst buf
    else
        buf


toRegisterText : Bool -> String -> RegisterText
toRegisterText linewise s =
    if linewise then
        Lines
            (if String.endsWith B.lineBreak s then
                s
             else
                s ++ B.lineBreak
            )
    else
        Text s


saveLastDeleted : Bool -> String -> Buffer -> Global -> Global
saveLastDeleted linewise reg buf global =
    let
        s =
            buf
                |> Buf.getLastDeleted
                |> Maybe.map B.toString
                |> Maybe.withDefault ""
                |> toRegisterText linewise
    in
        Buf.setRegister reg s global


join : Maybe Int -> Bool -> Buffer -> Buffer
join count collapseSpaces buf =
    let
        ( y, x ) =
            buf.view.cursor
    in
        case buf.mode of
            Visual { begin, end } ->
                let
                    yb =
                        Tuple.first begin

                    ye =
                        Tuple.first end

                    lineRange =
                        if yb == ye then
                            [ yb ]
                        else
                            List.repeat (abs (yb - ye)) (min yb ye)
                in
                    joinLines collapseSpaces lineRange buf

            _ ->
                joinLines collapseSpaces
                    (List.repeat
                        (count
                            |> Maybe.map (\n -> max (n - 1) 1)
                            |> Maybe.withDefault 1
                        )
                        y
                    )
                    buf


joinLines : Bool -> List Int -> Buffer -> Buffer
joinLines collapseSpaces lineRange buf =
    List.foldl (joinHelper collapseSpaces) buf lineRange


joinHelper : Bool -> Int -> Buffer -> Buffer
joinHelper collapseSpaces y buf =
    Maybe.map2
        (\line nextLine ->
            let
                lineEnd =
                    String.length line

                newCursor =
                    ( y, lineEnd - 1 )

                patches =
                    if collapseSpaces then
                        [ Deletion
                            ( y, lineEnd - 1 )
                            ( y + 1
                            , findLineFirst nextLine
                            )
                        ]
                            ++ if
                                (line == B.lineBreak)
                                    || (String.endsWith
                                            (" " ++ B.lineBreak)
                                            line
                                       )
                               then
                                []
                               else
                                [ Insertion
                                    ( y, lineEnd - 1 )
                                    (B.fromString " ")
                                ]
                    else
                        [ Deletion
                            ( y, lineEnd - 1 )
                            ( y, lineEnd )
                        ]
            in
                buf
                    |> Buf.transaction patches
                    |> Buf.updateView (Buf.setCursor newCursor True)
        )
        (B.getLine y buf.lines)
        (B.getLine (y + 1) buf.lines)
        |> Maybe.withDefault buf
