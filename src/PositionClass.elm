module PositionClass exposing (findPosition, parserWordEdge)

import Char
import Parser as P exposing (Parser, (|.), (|=))
import Internal.TextBuffer as B
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


spaceInline : Char -> Bool
spaceInline char =
    (Char.toCode char < 20 || char == ' ') && char /= '\n'


parserWordStart : String -> Bool -> Parser Int
parserWordStart wordChars crossLine =
    P.succeed
        (length3 -1)
        |= P.oneOf
            [ P.keep P.oneOrMore (word wordChars)
            , P.keep P.oneOrMore (punctuation wordChars)
            , P.keep P.oneOrMore space
            ]
        |= P.keep P.zeroOrMore space
        |= P.oneOf
            ([ P.keep (P.Exactly 1) (word wordChars)
             , P.keep (P.Exactly 1) (punctuation wordChars)
             ]
                ++ (if crossLine then
                        []
                    else
                        [ P.end |> P.map (always "") ]
                   )
            )


parserLineStart : Parser Int
parserLineStart =
    P.succeed
        (length1 0)
        |= P.keep P.zeroOrMore space
        |. P.oneOf
            [ P.ignore (P.Exactly 1) (space >> not)
            , P.end
            ]


parserWordEnd : String -> Parser Int
parserWordEnd wordChars =
    P.succeed
        (length2 0)
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


parserWORDStart : Bool -> Parser Int
parserWORDStart crossLine =
    P.succeed
        (length3 -1)
        |= P.oneOf
            [ P.keep P.oneOrMore (space >> not)
            , P.keep P.oneOrMore space
            ]
        |= P.keep P.zeroOrMore space
        |= (if crossLine then
                P.keep (P.Exactly 1) (space >> not)
            else
                P.oneOf
                    [ P.keep (P.Exactly 1) (space >> not)
                    , P.end |> P.map (always "")
                    ]
           )


length1 : Int -> String -> Int
length1 delta a =
    delta + String.length a


length2 : Int -> String -> String -> Int
length2 delta a b =
    delta + String.length a + String.length b


length3 : Int -> String -> String -> String -> Int
length3 delta a b c =
    delta
        + String.length a
        + String.length b
        + String.length c


parserWordEdge : String -> Parser Int
parserWordEdge wordChars =
    let
        divider pred =
            P.succeed
                (length2 -1)
                |= P.keep P.oneOrMore pred
                |= P.oneOf
                    [ P.keep (P.Exactly 1) (pred >> not)
                    , P.end |> P.map (always "")
                    ]
    in
        P.oneOf
            [ divider space
            , divider (word wordChars)
            , divider (punctuation wordChars)
            ]


parserWordAround : String -> Parser Int
parserWordAround wordChars =
    parserWordStart wordChars False


parserWORDAround : Parser Int
parserWORDAround =
    P.oneOf
        [ P.succeed
            (length1 -1)
            |= P.keep P.oneOrMore spaceInline
            |. P.oneOf
                [ P.ignore (P.Exactly 1) (spaceInline >> not)
                , P.end
                ]
        , P.succeed
            (length2 -1)
            |= P.keep P.oneOrMore (space >> not)
            |= P.keep P.zeroOrMore spaceInline
        ]


parserWORDEdge : Parser Int
parserWORDEdge =
    P.oneOf
        [ P.succeed
            (length1 -1)
            |= P.keep P.oneOrMore space
            |. P.oneOf
                [ P.ignore (P.Exactly 1) (space >> not)
                , P.end
                ]
        , P.succeed
            (length1 -1)
            |= P.keep P.oneOrMore (space >> not)
            |. P.oneOf
                [ P.ignore (P.Exactly 1) space
                , P.end
                ]
        ]


parserWORDEnd : Parser Int
parserWORDEnd =
    P.succeed
        (length2 0)
        |. P.keep (P.Exactly 1) (always True)
        |= P.keep P.zeroOrMore space
        |= P.keep P.oneOrMore (space >> not)
        |. P.oneOf
            [ P.ignore (P.Exactly 1) space
            , P.end
            ]


parserChar : Char -> Parser Int
parserChar ch =
    P.succeed
        (length2 0)
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
                    parserWordStart wordChars True

                WordStart ->
                    parserWordEnd wordChars

                WORDEnd ->
                    parserWORDStart True

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
    -> Bool
    -> String
    -> Result P.Error Int
findPositionForward wordChars md crossLine line =
    let
        parser =
            case md of
                WordEnd ->
                    parserWordEnd wordChars

                WordStart ->
                    parserWordStart wordChars crossLine

                WORDEnd ->
                    parserWORDEnd

                WORDStart ->
                    parserWORDStart crossLine

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
findPosition wordChars md mo line pos =
    let
        line1 =
            if String.endsWith B.lineBreak line then
                line
            else
                line ++ B.lineBreak
    in
        if mo.forward then
            case md of
                LineEnd ->
                    Just (String.length line1 - 1)

                _ ->
                    line1
                        |> String.dropLeft pos
                        |> findPositionForward wordChars md mo.crossLine
                        --|> Debug.log "result"
                        |> Result.toMaybe
                        |> Maybe.map ((+) pos)
        else
            case md of
                LineStart ->
                    Just 0

                LineFirst ->
                    P.run parserLineStart line1
                        |> Result.toMaybe

                _ ->
                    line1
                        |> String.left (pos + 1)
                        |> findPositionBackward wordChars md
                        --|> Debug.log "result"
                        |> Result.toMaybe
