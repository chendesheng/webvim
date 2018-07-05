module Internal.PositionClass
    exposing
        ( findPosition
        , findLineFirst
        , parserWordEdge
        , parserForwardCharRespectBackslash
        , parserBackwardCharRespectBackslash
        )

import Char
import Parser as P exposing (Parser, (|.), (|=))
import Internal.TextBuffer as B
import Vim.AST
    exposing
        ( MotionData(..)
        , MotionOption
        , Direction(..)
        , TextObject(..)
        , motionOption
        )
import Maybe
import Helper.Helper exposing (isSpace, notSpace, isBetween, word)


punctuation : String -> Char -> Bool
punctuation wordChars char =
    not (word wordChars char) && not (isSpace char)


spaceInline : Char -> Bool
spaceInline char =
    (Char.toCode char < 20 || char == ' ') && char /= '\n'


parserWordStart : String -> Bool -> Parser Int
parserWordStart wordChars crossLine =
    P.succeed
        (sumLength3 -1)
        |= P.oneOf
            [ P.keep P.oneOrMore (word wordChars)
            , P.keep P.oneOrMore (punctuation wordChars)
            , P.keep P.oneOrMore isSpace
            ]
        |= P.keep P.zeroOrMore isSpace
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


parserLineFirst : Parser Int
parserLineFirst =
    P.succeed
        (sumLength1 0)
        |= P.keep P.zeroOrMore spaceInline
        |. P.oneOf
            [ P.ignore (P.Exactly 1) (spaceInline >> not)
            , P.end
            ]


parserWordEnd : String -> Parser Int
parserWordEnd wordChars =
    P.succeed
        (sumLength2 0)
        |. P.keep (P.Exactly 1) (always True)
        |= P.keep P.zeroOrMore isSpace
        |= P.oneOf
            [ P.keep P.oneOrMore (word wordChars)
            , P.keep P.oneOrMore (punctuation wordChars)
            ]
        |. P.oneOf
            [ P.ignore (P.Exactly 1) (word wordChars)
            , P.ignore (P.Exactly 1) (punctuation wordChars)
            , P.ignore (P.Exactly 1) isSpace
            , P.end
            ]


parserWORDStart : Bool -> Parser Int
parserWORDStart crossLine =
    P.succeed
        (sumLength3 -1)
        |= P.oneOf
            [ P.keep P.oneOrMore notSpace
            , P.keep P.oneOrMore isSpace
            ]
        |= P.keep P.zeroOrMore isSpace
        |= (if crossLine then
                P.keep (P.Exactly 1) notSpace
            else
                P.oneOf
                    [ P.keep (P.Exactly 1) notSpace
                    , P.end |> P.map (always "")
                    ]
           )


sumLength1 : Int -> String -> Int
sumLength1 delta a =
    delta + String.length a


sumLength2 : Int -> String -> String -> Int
sumLength2 delta a b =
    delta + String.length a + String.length b


sumLength3 : Int -> String -> String -> String -> Int
sumLength3 delta a b c =
    delta
        + String.length a
        + String.length b
        + String.length c


parserWordEdge : String -> Parser Int
parserWordEdge wordChars =
    let
        divider pred =
            P.keep P.oneOrMore pred
                |. P.oneOf
                    [ P.keep (P.Exactly 1) (pred >> not)
                    , P.end |> P.map (always "")
                    ]
    in
        P.succeed String.length
            |= P.oneOf
                [ divider spaceInline
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
            (sumLength1 -1)
            |= P.keep P.oneOrMore spaceInline
            |. P.oneOf
                [ P.ignore (P.Exactly 1) (spaceInline >> not)
                , P.end
                ]
        , P.succeed
            (sumLength2 -1)
            |= P.keep P.oneOrMore notSpace
            |= P.keep P.zeroOrMore spaceInline
        ]


parserWORDEdge : Parser Int
parserWORDEdge =
    let
        divider pred =
            P.succeed
                String.length
                |= P.keep P.oneOrMore pred
                |. P.oneOf
                    [ P.keep (P.Exactly 1) (pred >> not)
                    , P.end |> P.map (always "")
                    ]
    in
        P.oneOf
            [ divider spaceInline
            , divider notSpace
            ]


parserWORDEnd : Parser Int
parserWORDEnd =
    P.succeed
        (sumLength2 0)
        |. P.keep (P.Exactly 1) (always True)
        |= P.keep P.zeroOrMore isSpace
        |= P.keep P.oneOrMore notSpace
        |. P.oneOf
            [ P.ignore (P.Exactly 1) isSpace
            , P.end
            ]


parserChar : Char -> Parser Int
parserChar ch =
    P.succeed
        (sumLength2 0)
        |= P.keep (P.Exactly 1) (always True)
        |= P.keep P.zeroOrMore ((/=) ch)
        |. P.ignore (P.Exactly 1) ((==) ch)


parserBeforeChar : Char -> Parser Int
parserBeforeChar ch =
    P.succeed String.length
        |. P.ignore (P.Exactly 1) (always True)
        |= P.keep P.zeroOrMore ((/=) ch)
        |. P.ignore (P.Exactly 1) ((==) ch)


parserForwardCharRespectBackslash : Char -> Parser Int
parserForwardCharRespectBackslash ch =
    P.succeed
        (+)
        |= (P.succeed String.length
                |= P.keep P.zeroOrMore (\c -> c /= '\\' && c /= ch)
           )
        |= P.oneOf
            [ P.succeed ((+) 2)
                |. P.ignore (P.Exactly 1) ((==) '\\')
                |. P.ignore (P.Exactly 1) (always True)
                |= P.lazy (\_ -> parserForwardCharRespectBackslash ch)
            , P.succeed 0
                |. P.ignore (P.Exactly 1) ((==) ch)
            ]


ignoreFirstChar : Parser Int -> Parser Int
ignoreFirstChar p =
    P.succeed ((+) 1)
        |. P.ignore (P.Exactly 1) (always True)
        |= p


parserBackwardCharRespectBackslash : Char -> Parser Int
parserBackwardCharRespectBackslash ch =
    P.succeed
        (+)
        |= (P.succeed String.length
                |= P.keep P.zeroOrMore ((/=) ch)
           )
        |. P.ignore (P.Exactly 1) ((==) ch)
        |= P.oneOf
            [ P.succeed ((+) 2)
                |. P.ignore (P.Exactly 1) ((==) '\\')
                |= P.lazy (\_ -> parserBackwardCharRespectBackslash ch)
            , P.succeed 0
            ]


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

                WordEdge ->
                    parserWordEdge wordChars

                WORDEdge ->
                    parserWORDEdge

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

                QuoteChar ch ->
                    parserBackwardCharRespectBackslash ch
                        |> ignoreFirstChar

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

                QuoteChar ch ->
                    parserForwardCharRespectBackslash ch

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

                CharStart ->
                    if pos + 1 >= String.length line1 then
                        Nothing
                    else
                        Just (pos + 1)

                _ ->
                    line1
                        |> String.dropLeft pos
                        |> findPositionForward wordChars md mo.crossLine
                        |> Result.toMaybe
                        |> Maybe.map ((+) pos)
        else
            case md of
                LineStart ->
                    Just 0

                LineFirst ->
                    P.run parserLineFirst line1
                        |> Result.toMaybe

                CharStart ->
                    if pos - 1 >= 0 then
                        Just (pos - 1)
                    else
                        Nothing

                _ ->
                    line1
                        |> String.left (pos + 1)
                        |> findPositionBackward wordChars md
                        |> Result.toMaybe


findLineFirst : String -> Int
findLineFirst line =
    P.run parserLineFirst line
        |> Result.withDefault 0
