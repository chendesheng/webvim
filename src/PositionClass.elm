module PositionClass exposing (..)

import Char
import Parser as P exposing (Parser, (|.), (|=))
import Vim.AST exposing (PositionClass(..))


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
            [ P.keep (P.Exactly 1) (word wordChars)
            , P.keep (P.Exactly 1) (punctuation wordChars)
            ]


parserWordEnd : String -> Parser Int
parserWordEnd wordChars =
    P.succeed
        (\a b -> String.length a + String.length b - 1)
        |= P.keep P.zeroOrMore space
        |= P.oneOf
            [ P.keep P.oneOrMore (word wordChars)
            , P.keep P.oneOrMore (punctuation wordChars)
            ]
        |. P.oneOf
            [ P.keep (P.Exactly 1) (word wordChars)
            , P.keep (P.Exactly 1) (punctuation wordChars)
            , P.keep (P.Exactly 1) space
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
        |. P.keep (P.Exactly 1) (space >> not)


parserWORDEnd : Parser Int
parserWORDEnd =
    P.succeed
        (\a b -> String.length a + String.length b - 1)
        |= P.keep P.zeroOrMore space
        |= P.keep P.oneOrMore (space >> not)
        |. P.keep (P.Exactly 1) space


findLastPositionClass : String -> PositionClass -> String -> Maybe Int
findLastPositionClass wordChars class line =
    let
        reversed =
            String.reverse line
    in
        (case class of
            WordStart ->
                -- word*space*(punctuation+|word+)
                Nothing

            _ ->
                Nothing
        )
            |> Maybe.map ((-) (String.length reversed))


findPositionClass : String -> PositionClass -> String -> Maybe Int
findPositionClass wordChars class line =
    case class of
        WordStart ->
            -- word*space*(punctuation+|word+)
            Nothing

        _ ->
            Nothing
