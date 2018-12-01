module Update.Replace exposing (applyReplace)

import Vim.AST as V exposing (Operator(..), ChangeCase(..))
import Model exposing (..)
import Update.Buffer as Buf
import Internal.TextBuffer as B exposing (Patch(..))
import Internal.Position exposing (Position)
import Update.Insert exposing (..)
import Update.Range exposing (operatorRanges)
import Regex as Re
import Helper.Helper exposing (regex)


replaceChar : String -> Buffer -> Buffer
replaceChar ch buf =
    let
        ( y, x ) =
            buf.cursor
    in
        buf
            |> Buf.transaction
                [ Deletion buf.cursor ( y, x + 1 ) ]
            |> insert (V.TextLiteral ch)


replaceRegion : String -> Position -> Position -> Buffer -> Buffer
replaceRegion ch b e buf =
    let
        s =
            buf.lines
                |> B.sliceRegion b e
                |> B.toString
                |> Re.replace (regex "[^\u{000D}\n]") (always ch)
                |> B.fromString
    in
        buf
            |> Buf.setCursor b False
            |> Buf.transaction
                [ Deletion b e
                , Insertion b s
                ]
            |> Buf.setCursor ( Tuple.first b, Tuple.second b + 1 ) True


applyReplace : Maybe Int -> String -> Global -> Buffer -> Buffer
applyReplace count ch global buf =
    case buf.mode of
        Normal _ ->
            buf
                |> replaceChar ch

        TempNormal ->
            buf
                |> replaceChar ch
                |> (\buf_ ->
                        let
                            ( y, x ) =
                                buf_.cursor
                        in
                            Buf.setCursor ( y, max 0 (x - 1) ) True buf_
                   )

        Visual _ ->
            let
                regions =
                    operatorRanges count (V.VisualRange False) global buf
                        |> List.reverse
            in
                List.foldl
                    (\( b, e ) -> replaceRegion ch b e )
                    buf
                    regions

        _ ->
            buf
