module Internal.PositionClass
    exposing
        ( findPosition
        , findLineFirst
        , parserWordEdge
        , parserForwardCharRespectBackslash
        , parserBackwardCharRespectBackslash
        )

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
import Helper.Helper
    exposing
        ( isSpace
        , notSpace
        , isBetween
        , word
        , regex
        , oneOrMore
        , keepOneOrMore
        )
import Regex as Re


punctuation : String -> Char -> Bool
punctuation wordChars char =
    not (word wordChars char) && not (isSpace char)


spaceInline : Char -> Bool
spaceInline char =
    isSpace char && char /= '\n'


parserWordStart : String -> Bool -> Parser Int
parserWordStart wordChars crossLine =
    P.succeed
        (sumLength3 -1)
        |= P.getChompedString
            (P.oneOf
                [ oneOrMore (word wordChars)
                , oneOrMore (punctuation wordChars)
                , oneOrMore isSpace
                ]
            )
        |= (P.getChompedString <| P.chompWhile isSpace)
        |= (P.getChompedString <|
                P.oneOf
                    ([ P.chompIf (word wordChars)
                     , P.chompIf (punctuation wordChars)
                     ]
                        ++ (if crossLine then
                                []
                            else
                                [ P.end ]
                           )
                    )
           )


parserLineFirst : Parser Int
parserLineFirst =
    P.succeed
        (sumLength1 0)
        |= P.getChompedString (P.chompWhile spaceInline)
        |. P.oneOf
            [ P.chompIf (spaceInline >> not)
            , P.end
            ]


parserWordEnd : String -> Parser Int
parserWordEnd wordChars =
    P.succeed
        (sumLength2 0)
        |. P.chompIf (always True)
        |= P.getChompedString (P.chompWhile isSpace)
        |= (P.oneOf
                [ oneOrMore (word wordChars)
                , oneOrMore (punctuation wordChars)
                ]
                |> P.getChompedString
           )
        |. P.oneOf
            [ P.chompIf (word wordChars)
            , P.chompIf (punctuation wordChars)
            , P.chompIf isSpace
            , P.end
            ]


parserWORDStart : Bool -> Parser Int
parserWORDStart crossLine =
    P.succeed
        (sumLength3 -1)
        |= (P.oneOf
                [ oneOrMore notSpace
                , oneOrMore isSpace
                ]
                |> P.getChompedString
           )
        |= P.getChompedString (P.chompIf isSpace)
        |= P.getChompedString
            (if crossLine then
                P.chompIf notSpace
             else
                P.oneOf
                    [ P.chompIf notSpace
                    , P.end
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
            oneOrMore pred
                |. P.oneOf
                    [ P.chompIf (pred >> not)
                    , P.end
                    ]
    in
        P.succeed String.length
            |= (P.oneOf
                    [ divider spaceInline
                    , divider (word wordChars)
                    , divider (punctuation wordChars)
                    ]
                    |> P.getChompedString
               )


parserWordAround : String -> Parser Int
parserWordAround wordChars =
    parserWordStart wordChars False


parserWORDAround : Parser Int
parserWORDAround =
    P.oneOf
        [ P.succeed
            (sumLength1 -1)
            |= keepOneOrMore spaceInline
            |. P.oneOf
                [ P.chompIf (spaceInline >> not)
                , P.end
                ]
        , P.succeed
            (sumLength2 -1)
            |= keepOneOrMore notSpace
            |= P.getChompedString (P.chompWhile spaceInline)
        ]


parserWORDEdge : Parser Int
parserWORDEdge =
    let
        divider pred =
            P.succeed
                String.length
                |= keepOneOrMore pred
                |. P.oneOf
                    [ P.chompIf (pred >> not)
                    , P.end
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
        |. P.chompIf (always True)
        |= P.getChompedString (P.chompWhile isSpace)
        |= keepOneOrMore notSpace
        |. P.oneOf
            [ P.chompIf isSpace
            , P.end
            ]


parserChar : Char -> Parser Int
parserChar ch =
    P.succeed
        (sumLength2 0)
        |= P.getChompedString (P.chompIf (always True))
        |= P.getChompedString (P.chompWhile ((/=) ch))
        |. P.chompIf ((==) ch)


parserBeforeChar : Char -> Parser Int
parserBeforeChar ch =
    P.succeed String.length
        |. P.chompIf (always True)
        |= P.getChompedString (P.chompWhile ((/=) ch))
        |. P.chompIf ((==) ch)


parserForwardCharRespectBackslash : Char -> Parser Int
parserForwardCharRespectBackslash ch =
    P.succeed
        (+)
        |= (P.succeed (\b e -> e - b)
                |= P.getOffset
                |. P.chompWhile (\c -> c /= '\\' && c /= ch)
                |= P.getOffset
           )
        |= P.oneOf
            [ P.succeed ((+) 2)
                |. P.chompIf ((==) '\\')
                |. P.chompIf (always True)
                |= P.lazy (\_ -> parserForwardCharRespectBackslash ch)
            , P.succeed 0
                |. P.chompIf ((==) ch)
            ]


ignoreFirstChar : Parser Int -> Parser Int
ignoreFirstChar p =
    P.succeed ((+) 1)
        |. P.chompIf (always True)
        |= p


parserBackwardCharRespectBackslash : Char -> Parser Int
parserBackwardCharRespectBackslash ch =
    P.succeed
        (+)
        |= (P.succeed (\b e -> e - b)
                |= P.getOffset
                |. P.chompWhile ((/=) ch)
                |= P.getOffset
           )
        |. P.chompIf ((==) ch)
        |= P.oneOf
            [ P.succeed ((+) 2)
                |. P.chompIf ((==) '\\')
                |= P.lazy (\_ -> parserBackwardCharRespectBackslash ch)
            , P.succeed 0
            ]


findPositionBackward :
    String
    -> MotionData
    -> String
    -> Result (List P.DeadEnd) Int
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
                            |> Maybe.withDefault (P.problem "invalid char")

                QuoteChar ch ->
                    parserBackwardCharRespectBackslash ch
                        |> ignoreFirstChar

                _ ->
                    P.problem "unknown position md"
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
    -> Result (List P.DeadEnd) Int
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
                            |> Maybe.withDefault (P.problem "invalid char")

                QuoteChar ch ->
                    parserForwardCharRespectBackslash ch

                _ ->
                    P.problem "unknown motion data "
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
    line
        |> Re.findAtMost 1 (regex "\\S|$|\n")
        |> List.head
        |> Maybe.map .index
        |> Maybe.withDefault 0
