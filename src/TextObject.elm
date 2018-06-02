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
import PositionClass exposing (findPosition, findLineFirst)


expandSingleLineTextObject :
    String
    -> TextObject
    -> Bool
    -> String
    -> Int
    -> Maybe ( Int, Int )
expandSingleLineTextObject wordChars textobj around line cursor =
    case textobj of
        Word ->
            Maybe.map2 (,)
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


wordUnderCursor : String -> Position -> B.TextBuffer -> Maybe ( Position, Position )
wordUnderCursor wordChars cursor lines =
    let
        ( y, x ) =
            cursor
    in
        B.getLine y lines
            |> Maybe.andThen
                (\line ->
                    (findPosition
                        wordChars
                        WordEnd
                        (motionOption ">]$-")
                        line
                        (Basics.max 0 (x - 1))
                    )
                        |> Maybe.andThen
                            (\end ->
                                (findPosition
                                    wordChars
                                    WordStart
                                    (motionOption "<]$-")
                                    line
                                    (end + 1)
                                )
                                    |> Maybe.map
                                        (\begin -> ( ( y, begin ), ( y, end + 1 ) ))
                            )
                )


wORDUnderCursor : Position -> B.TextBuffer -> Maybe ( Position, Position )
wORDUnderCursor cursor lines =
    let
        ( y, x ) =
            cursor
    in
        B.getLine y lines
            |> Maybe.andThen
                (\line ->
                    (findPosition
                        ""
                        WORDEnd
                        (motionOption ">]$-")
                        line
                        (Basics.max 0 (x - 1))
                    )
                        |> Maybe.andThen
                            (\end ->
                                (findPosition
                                    ""
                                    WORDStart
                                    (motionOption "<]$-")
                                    line
                                    (end + 1)
                                )
                                    |> Maybe.map
                                        (\begin -> ( ( y, begin ), ( y, end + 1 ) ))
                            )
                )


expandTextObject :
    String
    -> TextObject
    -> Bool
    -> B.TextBuffer
    -> Position
    -> Maybe ( Position, Position )
expandTextObject wordChars textObject around lines ( y, x ) =
    case textObject of
        Line ->
            if around then
                Just ( ( y, 0 ), ( y + 1, 0 ) )
            else
                B.getLine y lines
                    |> Maybe.map
                        (\line ->
                            ( ( y, findLineFirst line )
                            , ( y + 1, 0 )
                            )
                        )

        _ ->
            B.getLine y lines
                |> Maybe.andThen
                    (\line ->
                        (expandSingleLineTextObject wordChars
                            textObject
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
                            Just ( ( y, a ), ( y, b + 1 ) )
                    )
