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
        , keepZeroOrMore
        , spaceInline
        )
import Regex as Re


punctuation : String -> Char -> Bool
punctuation wordChars char =
    not (word wordChars char) && not (isSpace char)


parserWordStart : String -> Bool -> Parser Int
parserWordStart wordChars crossLine =
    P.succeed (\b e -> e - b - 1)
        |= P.getOffset
        |. P.oneOf
            [ oneOrMore (word wordChars)
            , oneOrMore (punctuation wordChars)
            , oneOrMore isSpace
            ]
        |. P.chompWhile isSpace
        |. P.oneOf
            ([ P.chompIf (word wordChars)
             , P.chompIf (punctuation wordChars)
             ]
                ++ (if crossLine then
                        []
                    else
                        [ P.end ]
                   )
            )
        |= P.getOffset


parserLineFirst : Parser Int
parserLineFirst =
    P.succeed
        (\b e -> e - b)
        |= P.getOffset
        |. P.chompWhile spaceInline
        |= P.getOffset


parserWordEnd : String -> Parser Int
parserWordEnd wordChars =
    P.succeed
        (\b e -> e - b)
        |. P.chompIf (always True)
        |= P.getOffset
        |. P.chompWhile isSpace
        |. P.oneOf
            [ oneOrMore (word wordChars)
            , oneOrMore (punctuation wordChars)
            ]
        |= P.getOffset


parserWORDStart : Bool -> Parser Int
parserWORDStart crossLine =
    P.succeed (\b e -> e - b - 1)
        |= P.getOffset
        |. P.oneOf
            [ oneOrMore notSpace
            , oneOrMore isSpace
            ]
        |. P.chompWhile isSpace
        |. (if crossLine then
                P.chompIf notSpace
            else
                P.oneOf
                    [ P.chompIf notSpace
                    , P.end
                    ]
           )
        |= P.getOffset


parserWordAround : String -> Parser Int
parserWordAround wordChars =
    parserWordStart wordChars False


parserWORDAround : Parser Int
parserWORDAround =
    P.oneOf
        [ P.succeed
            (\b e -> e - b - 1)
            |= P.getOffset
            |. oneOrMore spaceInline
            |. P.oneOf
                [ P.chompIf (spaceInline >> not)
                , P.end
                ]
            |= P.getOffset
        , P.succeed
            (\b e -> e - b - 1)
            |= P.getOffset
            |. oneOrMore notSpace
            |. P.chompWhile spaceInline
            |= P.getOffset
        ]


parserWordEdge : String -> Parser Int
parserWordEdge wordChars =
    P.succeed (\b e -> e - b)
        |= P.getOffset
        |. P.oneOf
            [ oneOrMore spaceInline
            , oneOrMore <| word wordChars
            , oneOrMore <| punctuation wordChars
            ]
        |= P.getOffset


parserWORDEdge : Parser Int
parserWORDEdge =
    P.succeed (\b e -> e - b)
        |= P.getOffset
        |. P.oneOf
            [ oneOrMore spaceInline
            , oneOrMore notSpace
            ]
        |= P.getOffset


parserWORDEnd : Parser Int
parserWORDEnd =
    P.succeed
        (\b e -> e - b)
        |. P.chompIf (always True)
        |= P.getOffset
        |. P.chompWhile isSpace
        |. oneOrMore notSpace
        |= P.getOffset


parserChar : Char -> Parser Int
parserChar ch =
    P.succeed
        (\b e -> e - b)
        |= P.getOffset
        |. P.chompIf (always True)
        |. P.chompWhile ((/=) ch)
        |= P.getOffset
        |. P.chompIf ((==) ch)


parserBeforeChar : Char -> Parser Int
parserBeforeChar ch =
    P.succeed (\b e -> e - b)
        |. P.chompIf (always True)
        |= P.getOffset
        |. keepZeroOrMore ((/=) ch)
        |= P.getOffset
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
