module Update.Increase exposing (increaseNumber)

import Char exposing (toLower, toUpper)
import Helper.Helper exposing (keepOneOrMore, keepZeroOrMore)
import Internal.TextBuffer as B exposing (Patch(..))
import Model exposing (..)
import Parser as P exposing ((|.), (|=), Parser)
import Update.Buffer as Buf


numParser : Parser ( Int, Int, Int )
numParser =
    P.succeed
        (\pre s ->
            ( String.length pre
            , String.length s
            , s
                |> String.toInt
                |> Maybe.withDefault 0
            )
        )
        |= keepZeroOrMore (not << Char.isDigit)
        |= keepOneOrMore Char.isDigit


increaseNumber : Maybe Int -> Bool -> Buffer -> Buffer
increaseNumber count larger buf =
    let
        ( y, x ) =
            buf.view.cursor

        delta =
            if larger then
                Maybe.withDefault 1 count

            else
                -(Maybe.withDefault 1 count)
    in
    buf.lines
        |> B.getLine y
        |> Maybe.andThen
            (\line ->
                line
                    |> String.slice x -1
                    |> P.run numParser
                    |> Result.toMaybe
                    |> Maybe.map
                        (\res ->
                            let
                                ( dx, len, n ) =
                                    res

                                isNegative =
                                    String.slice
                                        (x + dx - 1)
                                        (x + dx)
                                        line
                                        == "-"

                                dx1 =
                                    if isNegative then
                                        dx - 1

                                    else
                                        dx

                                n1 =
                                    if isNegative then
                                        -n

                                    else
                                        n

                                len1 =
                                    if isNegative then
                                        len + 1

                                    else
                                        len

                                cursor =
                                    ( y, x + dx1 )

                                patches =
                                    [ Deletion cursor
                                        ( y
                                        , x + dx1 + len1
                                        )
                                    , n1
                                        |> (+) delta
                                        |> String.fromInt
                                        |> B.fromString
                                        |> Insertion cursor
                                    ]
                            in
                            buf
                                |> Buf.transaction
                                    patches
                                |> Buf.updateView (Buf.setCursor cursor True)
                        )
            )
        |> Maybe.withDefault buf
