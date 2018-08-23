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


deleteOperator : Maybe Int -> V.OperatorRange -> Buffer -> List Patch
deleteOperator count range buf =
    let
        ranges =
            operatorRanges count range buf

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


delete : Maybe Int -> String -> V.OperatorRange -> Buffer -> Buffer
delete count register rg buf =
    let
        updateCursorColumn buf_ =
            { buf_ | cursorColumn = Tuple.second buf_.cursor }

        linewise =
            isLinewise rg buf.mode

        deleteAnd f buf_ =
            case deleteOperator count rg buf_ of
                [] ->
                    let
                        last =
                            buf_.last

                        setMotionFailed buf__ =
                            { buf__ | last = { last | motionFailed = True } }
                    in
                        case rg of
                            V.MotionRange md _ ->
                                case md of
                                    V.MatchString _ ->
                                        setMotionFailed buf_

                                    V.MatchChar _ _ ->
                                        setMotionFailed buf_

                                    _ ->
                                        buf_

                            _ ->
                                buf_

                patches ->
                    let
                        setCursor buf__ =
                            case getLast patches of
                                Just (Deletion b e) ->
                                    Buf.setCursor b True buf__

                                _ ->
                                    buf__
                    in
                        buf_
                            |> Buf.transaction patches
                            |> setCursor
                            |> f
    in
        case buf.mode of
            Ex ({ exbuf } as ex) ->
                -- FIXME: too hack!!
                case rg of
                    V.MotionRange md mo ->
                        case md of
                            V.MatchString _ ->
                                buf
                                    |> deleteAnd
                                        (saveLastDeleted linewise register
                                            >> updateCursorColumn
                                        )
                                    |> saveMotion md mo buf

                            _ ->
                                Buf.setMode
                                    (Ex
                                        { ex
                                            | exbuf = deleteAnd identity exbuf
                                        }
                                    )
                                    buf

                    _ ->
                        Buf.setMode
                            (Ex { ex | exbuf = deleteAnd identity exbuf })
                            buf

            Insert _ ->
                deleteAnd (Buf.setLastIndent 0) buf

            _ ->
                deleteAnd
                    (saveLastDeleted linewise register
                        >> indentCursor linewise
                        >> updateCursorColumn
                    )
                    buf


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


saveLastDeleted : Bool -> String -> Buffer -> Buffer
saveLastDeleted linewise reg buf =
    let
        s =
            buf
                |> Buf.getLastDeleted
                |> Maybe.map B.toString
                |> Maybe.withDefault ""
                |> toRegisterText linewise
    in
        Buf.setRegister reg s buf


join : Maybe Int -> Bool -> Buffer -> Buffer
join count collapseSpaces buf =
    let
        ( y, x ) =
            buf.cursor
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
                    |> Buf.setCursor newCursor True
        )
        (B.getLine y buf.lines)
        (B.getLine (y + 1) buf.lines)
        |> Maybe.withDefault buf
