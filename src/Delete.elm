module Delete
    exposing
        ( delete
        , join
        , toRegisterText
        )

import Range exposing (operatorRanges, isLinewise)
import Model exposing (..)
import Vim.AST as V exposing (Operator(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Buffer as Buf
import Position exposing (Position, positionMin)
import Motion exposing (..)
import PositionClass exposing (findLineFirst)
import List


type alias Transaction =
    { pos : Maybe ( Position, Bool )
    , patches : List Patch
    }


applyTransaction : Transaction -> Buffer -> Buffer
applyTransaction { pos, patches } buf =
    case pos of
        Just ( cursor, saveColumn ) ->
            buf
                |> Buf.setCursor cursor saveColumn
                |> Buf.transaction patches

        _ ->
            Buf.transaction patches buf


deleteOperator : V.OperatorRange -> Buffer -> Maybe Transaction
deleteOperator range buf =
    let
        ranges =
            operatorRanges range buf

        pos =
            case range of
                V.MotionRange md mo ->
                    case runMotion md mo buf of
                        Just pos ->
                            if mo.linewise then
                                Just ( pos, False )
                            else
                                Nothing

                        Nothing ->
                            Nothing

                V.VisualRange linewise ->
                    case buf.mode of
                        Visual { begin, end } ->
                            let
                                begin1 =
                                    min begin end
                            in
                                Just ( begin1, True )

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
                        _ ->
                            Nothing

                _ ->
                    Nothing
    in
        if List.isEmpty ranges then
            Nothing
        else
            Just
                { pos = pos
                , patches =
                    List.map
                        (\( begin, end ) ->
                            Deletion begin end
                        )
                        ranges
                }


delete : String -> V.OperatorRange -> Buffer -> Buffer
delete register rg buf =
    let
        updateCursorColumn buf =
            { buf | cursorColumn = Tuple.second buf.cursor }

        linewise =
            isLinewise rg buf.mode

        deleteAnd f buf =
            case deleteOperator rg buf of
                Just trans ->
                    buf
                        |> applyTransaction trans
                        |> f

                _ ->
                    buf
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
                                    |> saveMotion md mo

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

            Insert ->
                deleteAnd identity buf

            _ ->
                deleteAnd
                    (saveLastDeleted linewise register
                        >> updateCursorColumn
                    )
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


join : Bool -> Buffer -> Buffer
join collapseSpaces buf =
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
                joinLines collapseSpaces [ y ] buf


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
