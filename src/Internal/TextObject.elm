module Internal.TextObject exposing (..)

import Internal.TextBuffer as B
import Vim.AST
    exposing
        ( MotionData(..)
        , MotionOption
        , Direction(..)
        , TextObject(..)
        , motionOption
        )
import Internal.Position exposing (Position)
import Internal.PositionClass exposing (findPosition, findLineFirst)
import Internal.Brackets as Bracket exposing (bracket, pairBracket)
import Internal.Syntax as Syntax exposing (Syntax)


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

        WORD ->
            Maybe.map2 (,)
                ((findPosition
                    wordChars
                    WORDEdge
                    (motionOption "<]$-")
                    line
                    cursor
                 )
                 --|> Debug.log "resulta"
                )
                ((findPosition
                    wordChars
                    WORDEdge
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


findPair :
    Int
    -> Int
    -> Syntax
    -> String
    -> String
    -> Bool
    -> B.TextBuffer
    -> Position
    -> Maybe ( Position, Position )
findPair scrollTop height syntax openChar closeChar around lines (( y, x ) as cursor) =
    let
        c =
            lines
                |> B.getLine y
                |> Maybe.map (String.slice x (x + 1))

        pairChar c =
            pairBracket
                scrollTop
                (scrollTop + height)
                lines
                syntax
                cursor
                c

        genRegion p1 p2 =
            let
                ( y1, x1 ) =
                    Basics.min p1 p2

                ( y2, x2 ) =
                    Basics.max p1 p2
            in
                if around then
                    ( ( y1, x1 ), ( y2, x2 + 1 ) )
                else
                    ( ( y1, x1 + 1 ), ( y2, x2 ) )
    in
        if c == Just openChar then
            openChar
                |> pairChar
                |> Maybe.map (genRegion cursor)
        else if c == Just closeChar then
            closeChar
                |> pairChar
                |> Maybe.map (genRegion cursor)
        else
            Maybe.map2
                genRegion
                (pairBracket
                    scrollTop
                    (scrollTop + height)
                    lines
                    syntax
                    cursor
                    closeChar
                )
                (pairBracket
                    scrollTop
                    (scrollTop + height)
                    lines
                    syntax
                    cursor
                    openChar
                )


expandTextObject :
    String
    -> Int
    -> Int
    -> Syntax
    -> TextObject
    -> Bool
    -> B.TextBuffer
    -> Position
    -> Maybe ( Position, Position )
expandTextObject wordChars scrollTop height syntax textObject around lines (( y, x ) as cursor) =
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

        Pair c ->
            case c of
                '(' ->
                    findPair scrollTop height syntax "(" ")" around lines cursor

                '[' ->
                    findPair scrollTop height syntax "[" "]" around lines cursor

                '{' ->
                    findPair scrollTop height syntax "{" "}" around lines cursor

                _ ->
                    Nothing

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
