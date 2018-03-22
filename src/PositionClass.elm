module PositionClass exposing (findPosition)

import Char
import Parser as P exposing (Parser, (|.), (|=))
import Vim.AST
    exposing
        ( MotionData(..)
        , MotionOption
        , Direction(..)
        )


isBetween : Char -> Char -> Char -> Bool
isBetween low high char =
    let
        code =
            Char.toCode char
    in
        (code >= Char.toCode low) && (code <= Char.toCode high)


word : String -> Char -> Bool
word wordChars char =
    isBetween 'a' 'z' char
        || isBetween 'A' 'Z' char
        || isBetween '0' '9' char
        || String.any ((==) char) wordChars


punctuation : String -> Char -> Bool
punctuation wordChars char =
    not (word wordChars char) && not (space char)


space : Char -> Bool
space char =
    Char.toCode char < 20 || char == ' '


parserWordStart : String -> Parser Int
parserWordStart wordChars =
    P.succeed
        (\a b -> String.length a + String.length b)
        |= P.oneOf
            [ P.keep P.oneOrMore (word wordChars)
            , P.keep P.oneOrMore (punctuation wordChars)
            , P.keep P.oneOrMore space
            ]
        |= P.keep P.zeroOrMore space
        |. P.oneOf
            [ P.ignore (P.Exactly 1) (word wordChars)
            , P.ignore (P.Exactly 1) (punctuation wordChars)
            ]


parserLineStart : Parser Int
parserLineStart =
    P.succeed
        (\a -> String.length a)
        |= P.keep P.zeroOrMore space
        |. P.ignore (P.Exactly 1) (space >> not)


parserWordEnd : String -> Parser Int
parserWordEnd wordChars =
    P.succeed
        (\a b -> String.length a + String.length b)
        |. P.keep (P.Exactly 1) (always True)
        |= P.keep P.zeroOrMore space
        |= P.oneOf
            [ P.keep P.oneOrMore (word wordChars)
            , P.keep P.oneOrMore (punctuation wordChars)
            ]
        |. P.oneOf
            [ P.ignore (P.Exactly 1) (word wordChars)
            , P.ignore (P.Exactly 1) (punctuation wordChars)
            , P.ignore (P.Exactly 1) space
            , P.end
            ]


parserWORDStart : Parser Int
parserWORDStart =
    P.succeed
        (\a b -> String.length a + String.length b)
        |= P.oneOf
            [ P.keep P.oneOrMore (space >> not)
            , P.keep P.oneOrMore space
            ]
        |= P.keep P.zeroOrMore space
        |. P.ignore (P.Exactly 1) (space >> not)


parserWordEdge : String -> Parser Int
parserWordEdge wordChars =
    P.oneOf
        [ P.succeed
            String.length
            |. P.ignore (P.Exactly 1) space
            |= P.keep P.zeroOrMore space
            |. P.oneOf
                [ P.ignore (P.Exactly 1) (space >> not)
                , P.end
                ]
        , P.succeed
            String.length
            |. P.ignore (P.Exactly 1) (word wordChars)
            |= P.keep P.zeroOrMore (word wordChars)
            |. P.oneOf
                [ P.ignore (P.Exactly 1) (word wordChars >> not)
                , P.end
                ]
        , P.succeed
            String.length
            |. P.ignore (P.Exactly 1) (punctuation wordChars)
            |= P.keep P.zeroOrMore (punctuation wordChars)
            |. P.oneOf
                [ P.ignore (P.Exactly 1) (punctuation wordChars >> not)
                , P.end
                ]
        ]


parserWORDEdge : Parser Int
parserWORDEdge =
    P.oneOf
        [ P.succeed
            String.length
            |. P.ignore (P.Exactly 1) space
            |= P.keep P.zeroOrMore space
            |. P.oneOf
                [ P.ignore (P.Exactly 1) (space >> not)
                , P.end
                ]
        , P.succeed
            String.length
            |. P.ignore (P.Exactly 1) (space >> not)
            |= P.keep P.zeroOrMore (space >> not)
            |. P.oneOf
                [ P.ignore (P.Exactly 1) space
                , P.end
                ]
        ]


parserWORDEnd : Parser Int
parserWORDEnd =
    P.succeed
        (\a b -> String.length a + String.length b)
        |. P.keep (P.Exactly 1) (always True)
        |= P.keep P.zeroOrMore space
        |= P.keep P.oneOrMore (space >> not)
        |. P.oneOf
            [ P.ignore (P.Exactly 1) space
            , P.end
            ]


parserChar : Char -> Parser Int
parserChar ch =
    P.succeed (\a b -> String.length a + String.length b)
        |= P.keep (P.Exactly 1) (always True)
        |= P.keep P.zeroOrMore ((/=) ch)
        |. P.ignore (P.Exactly 1) ((==) ch)


parserBeforeChar : Char -> Parser Int
parserBeforeChar ch =
    P.succeed String.length
        |. P.ignore (P.Exactly 1) (always True)
        |= P.keep P.zeroOrMore ((/=) ch)
        |. P.ignore (P.Exactly 1) ((==) ch)


parserCharStart : Parser Int
parserCharStart =
    P.succeed String.length
        |. P.keep (P.Exactly 1) (always True)
        |= P.oneOf
            [ P.keep (P.Exactly 1) (always True) ]


findPositionBackward :
    String
    -> MotionData
    -> String
    -> Result P.Error Int
findPositionBackward wordChars md line =
    let
        parser =
            case md of
                WordEnd ->
                    parserWordStart wordChars

                WordStart ->
                    parserWordEnd wordChars

                WORDEnd ->
                    parserWORDStart

                WORDStart ->
                    parserWORDEnd

                CharStart ->
                    parserCharStart

                MatchChar ch before ->
                    let
                        p =
                            if before then
                                parserBeforeChar
                            else
                                parserChar
                    in
                        ch
                            |> String.uncons
                            |> Maybe.map (Tuple.first >> p)
                            |> Maybe.withDefault (P.fail "invalid char")

                _ ->
                    P.fail "unknown position md"
    in
        line
            |> String.reverse
            |> P.run parser
            |> Result.map (\n -> String.length line - n - 1)


findPositionForward :
    String
    -> MotionData
    -> String
    -> Result P.Error Int
findPositionForward wordChars md line =
    let
        parser =
            case md of
                WordEnd ->
                    parserWordEnd wordChars

                WordStart ->
                    parserWordStart wordChars

                WORDEnd ->
                    parserWORDEnd

                WORDStart ->
                    parserWORDStart

                WordEdge ->
                    parserWordEdge wordChars

                WORDEdge ->
                    parserWORDEdge

                CharStart ->
                    parserCharStart

                MatchChar ch before ->
                    let
                        p =
                            if before then
                                parserBeforeChar
                            else
                                parserChar
                    in
                        ch
                            |> String.uncons
                            |> Maybe.map (Tuple.first >> p)
                            |> Maybe.withDefault (P.fail "invalid char")

                _ ->
                    P.fail "unknown motion data "
    in
        P.run parser line


findPosition :
    String
    -> MotionData
    -> MotionOption
    -> String
    -> Int
    -> Maybe Int
findPosition wordChars md option line pos =
    if option.forward then
        if md == LineEnd then
            Just (String.length line - 1)
        else
            line
                |> String.dropLeft pos
                |> findPositionForward wordChars md
                --|> Debug.log "result"
                |> Result.toMaybe
                |> Maybe.map ((+) pos)
    else
        case md of
            LineStart ->
                Just 0

            LineFirst ->
                P.run parserLineStart line
                    |> Result.toMaybe

            _ ->
                line
                    |> String.left (pos + 1)
                    |> findPositionBackward wordChars md
                    --|> Debug.log "result"
                    |> Result.toMaybe
