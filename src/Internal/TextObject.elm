module Internal.TextObject exposing (expandSingleLineTextObject, expandTextObject, findPair, wORDUnderCursor, wordUnderCursor)

import Internal.Brackets exposing (pairBracket)
import Internal.Position exposing (Position)
import Internal.PositionClass exposing (findLineFirst, findPosition)
import Internal.Syntax exposing (Syntax)
import Internal.TextBuffer as B
import Vim.AST
    exposing
        ( Direction(..)
        , MotionData(..)
        , TextObject(..)
        , motionOption
        )


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
            Maybe.map2 (\a b -> ( a + 1, b - 1 ))
                (findPosition
                    wordChars
                    WordEdge
                    (motionOption "<)$-")
                    line
                    cursor
                 --|> Debug.log "resulta"
                )
                (findPosition
                    wordChars
                    WordEdge
                    (motionOption ">)$-")
                    line
                    cursor
                 --|> Debug.log ("resultb" ++ " " ++ toString (max 0 (cursor - 1)) ++ " " ++ line)
                )

        WORD ->
            Maybe.map2 (\a b -> ( a + 1, b - 1 ))
                (findPosition
                    wordChars
                    WORDEdge
                    (motionOption "<)$-")
                    line
                    cursor
                 --|> Debug.log "resulta"
                )
                (findPosition
                    wordChars
                    WORDEdge
                    (motionOption ">)$-")
                    line
                    cursor
                 --|> Debug.log ("resultb" ++ " " ++ toString (max 0 (cursor - 1)) ++ " " ++ line)
                )

        Quote c ->
            let
                a =
                    findPosition wordChars
                        (QuoteChar c)
                        (motionOption "<]$-")
                        line
                        cursor

                --|> Debug.log "resulta"
                b =
                    findPosition wordChars
                        (QuoteChar c)
                        (motionOption ">]$-")
                        line
                        cursor

                --|> Debug.log ("resultb" ++ " " ++ toString (max 0 (cursor - 1)) ++ " " ++ line)
                res =
                    case a of
                        Just i ->
                            case b of
                                Just j ->
                                    Just ( i, j )

                                _ ->
                                    Nothing

                        _ ->
                            case b of
                                Just j ->
                                    if cursor == j then
                                        -- cursor on top of ch and
                                        -- backward part not found
                                        -- try forward again
                                        findPosition wordChars
                                            (QuoteChar c)
                                            (motionOption ">]$-")
                                            line
                                            (cursor + 1)
                                            |> Maybe.map (Tuple.pair cursor)

                                    else
                                        Nothing

                                _ ->
                                    Nothing
            in
            Maybe.map
                (\( i, j ) ->
                    if around then
                        ( i, j )

                    else
                        ( i + 1, j - 1 )
                )
                res

        _ ->
            Nothing


wordUnderCursor :
    String
    -> Position
    -> B.TextBuffer
    -> Maybe ( Position, Position )
wordUnderCursor wordChars cursor lines =
    let
        ( y, x ) =
            cursor
    in
    B.getLine y lines
        |> Maybe.andThen
            (\line ->
                findPosition
                    wordChars
                    WordEnd
                    (motionOption ">]$-")
                    line
                    (Basics.max 0 (x - 1))
                    |> Maybe.andThen
                        (\end ->
                            findPosition
                                wordChars
                                WordStart
                                (motionOption "<]$-")
                                line
                                (end + 1)
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
                findPosition
                    ""
                    WORDEnd
                    (motionOption ">]$-")
                    line
                    (Basics.max 0 (x - 1))
                    |> Maybe.andThen
                        (\end ->
                            findPosition
                                ""
                                WORDStart
                                (motionOption "<]$-")
                                line
                                (end + 1)
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

        pairChar =
            pairBracket
                scrollTop
                (scrollTop + height)
                lines
                syntax
                cursor
                False

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
        Maybe.map
            (\( b, e ) -> genRegion b e)
            (pairBracket
                scrollTop
                (scrollTop + height)
                lines
                syntax
                cursor
                True
                closeChar
                |> Maybe.andThen
                    (\openPos ->
                        pairBracket
                            scrollTop
                            (scrollTop + height)
                            lines
                            syntax
                            openPos
                            False
                            openChar
                            |> Maybe.map (\closePos -> ( openPos, closePos ))
                    )
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
                        expandSingleLineTextObject wordChars
                            textObject
                            around
                            line
                            x
                    )
                |> Maybe.andThen
                    (\( a, b ) ->
                        Just ( ( y, a ), ( y, b + 1 ) )
                    )
