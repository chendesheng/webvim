module PositionClass exposing (findPosition)

import Char
import Parser as P exposing (Parser, (|.), (|=))
import Vim.AST exposing (PositionClass(..), Direction(..))


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


parserWordEnd : String -> Parser Int
parserWordEnd wordChars =
    P.succeed
        (\a b c -> String.length a + String.length b + String.length c - 1)
        |= P.keep (P.Exactly 1) (always True)
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


parserWORDEnd : Parser Int
parserWORDEnd =
    P.succeed
        (\a b c -> String.length a + String.length b + String.length c - 1)
        |= P.keep (P.Exactly 1) (always True)
        |= P.keep P.zeroOrMore space
        |= P.keep P.oneOrMore (space >> not)
        |. P.oneOf
            [ P.ignore (P.Exactly 1) space
            , P.end
            ]


findPositionBackward : String -> PositionClass -> String -> Maybe Int
findPositionBackward wordChars class line =
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


findPositionForward :
    String
    -> PositionClass
    -> String
    -> Result P.Error Int
findPositionForward wordChars class line =
    let
        parser =
            case class of
                WordEnd ->
                    parserWordEnd wordChars

                WordStart ->
                    parserWordStart wordChars

                WORDEnd ->
                    parserWORDEnd

                WORDStart ->
                    parserWORDStart

                _ ->
                    P.fail "unknown position class"
    in
        P.run parser line


findPosition :
    String
    -> PositionClass
    -> Direction
    -> String
    -> Int
    -> Maybe Int
findPosition wordChars class direction line pos =
    case direction of
        Forward ->
            line
                |> String.dropLeft pos
                |> findPositionForward wordChars class
                |> Debug.log "result"
                |> Result.toMaybe
                |> Maybe.map ((+) pos)

        Backward ->
            findPositionBackward wordChars class <|
                String.left pos line
