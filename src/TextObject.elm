module TextObject exposing (..)

import Internal.TextBuffer as B
import Vim.AST
    exposing
        ( MotionData(..)
        , MotionOption
        , Direction(..)
        , TextObject(..)
        , motionOption
        )
import Position exposing (Position)
import PositionClass exposing (findPosition)


expandTextObjectHelper :
    String
    -> TextObject
    -> Bool
    -> String
    -> Int
    -> Maybe ( Int, Int )
expandTextObjectHelper wordChars textobj around line cursor =
    if around then
        Nothing
    else
        case textobj of
            Word ->
                Maybe.map2 (\a b -> ( a, b ))
                    ((findPosition
                        wordChars
                        WordEdge
                        (motionOption "<]$-")
                        line
                        cursor
                     )
                     --|> Debug.log "resulta"
                    )
                    ((findPosition
                        wordChars
                        WordEdge
                        (motionOption ">]$-")
                        line
                        cursor
                     )
                     --|> Debug.log ("resultb" ++ " " ++ toString (max 0 (cursor - 1)) ++ " " ++ line)
                    )

            _ ->
                Nothing


expandTextObject :
    String
    -> TextObject
    -> Bool
    -> Position
    -> B.TextBuffer
    -> Maybe ( Position, Position )
expandTextObject wordChars textobj around ( y, x ) lines =
    B.getLine y lines
        |> Maybe.andThen
            (\line ->
                (expandTextObjectHelper wordChars
                    textobj
                    around
                    line
                    x
                )
            )
        |> Maybe.andThen
            (\rg ->
                let
                    ( a, b ) =
                        rg
                in
                    Just ( ( y, a ), ( y, b ) )
            )
