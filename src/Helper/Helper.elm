module Helper.Helper exposing (..)

import Dict exposing (Dict)
import Regex as Re exposing (Regex)
import Native.Doc
import Char
import Parser as P exposing ((|.), (|=))


getLast : List a -> Maybe a
getLast xs =
    case xs of
        [] ->
            Nothing

        [ x ] ->
            Just x

        x :: xs ->
            getLast xs


minMaybe : Maybe Int -> Maybe Int -> Maybe Int
minMaybe a b =
    if a == Nothing then
        b
    else if b == Nothing then
        a
    else
        Maybe.map2 Basics.min a b


fromListBy : (v -> comparable) -> List v -> Dict comparable v
fromListBy fnkey lst =
    lst
        |> List.map (\item -> ( fnkey item, item ))
        |> Dict.fromList


filename : String -> ( String, String )
filename s =
    case
        Re.find
            (Re.AtMost 1)
            (Re.regex "(^|[/\\\\])([^.]+)([.][^.]*)?$")
            s
    of
        [ m ] ->
            case m.submatches of
                [ _, a, b ] ->
                    ( Maybe.withDefault "" a
                    , Maybe.withDefault "" b
                    )

                _ ->
                    ( "", "" )

        _ ->
            ( "", "" )


repeatfn : number -> (a -> Maybe a) -> a -> Maybe a
repeatfn n f =
    let
        fn i arg =
            if i == 0 then
                Just arg
            else
                case f arg of
                    Just arg1 ->
                        fn (i - 1) arg1

                    _ ->
                        Nothing
    in
        fn n


safeRegex : String -> Maybe Regex
safeRegex s =
    if Native.Doc.checkRegex s then
        Just (Re.regex s)
    else
        Nothing


levenshtein : String -> String -> Int
levenshtein =
    Native.Doc.levenshtein


isSpace c =
    Char.toCode c < 20 || c == ' '


notSpace =
    isSpace >> not


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


parseWords : String -> String -> List String
parseWords wordChars s =
    s
        |> P.run
            (P.oneOf
                [ P.keep P.oneOrMore
                    (word wordChars)
                , P.keep P.oneOrMore
                    (word wordChars >> not)
                    |> P.map (always "")
                ]
                |> P.repeat P.oneOrMore
            )
        |> Result.withDefault []
        |> List.filter (\s -> String.length s >= 2)


maybeAndThen2 : (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
maybeAndThen2 f ma mb =
    Maybe.andThen
        (\a ->
            Maybe.andThen (f a) mb
        )
        ma
