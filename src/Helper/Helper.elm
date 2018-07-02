module Helper.Helper exposing (..)

import Dict exposing (Dict)
import Regex as Re exposing (Regex)
import Native.Doc
import Char
import Parser as P exposing ((|.), (|=))
import Elm.Array as Array exposing (Array)


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
            (Re.regex "(^|[/\\\\])([^./\\\\]+)([.][^.]*)?$")
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


isSpace : Char -> Bool
isSpace c =
    Char.toCode c < 20 || c == ' '


notSpace : Char -> Bool
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


isAbsolutePath : String -> String -> Bool
isAbsolutePath sep =
    if sep == "/" then
        String.startsWith "/"
    else
        Re.contains (Re.regex "^[a-zA-Z]:\\\\")


joinPath : String -> String -> String -> String
joinPath sep a b =
    let
        _ =
            Debug.log "joinPath" ( sep, a, b )
    in
        if isAbsolutePath sep b then
            b
        else if String.endsWith sep a then
            a ++ b
        else
            a ++ sep ++ b


dropWhile : (a -> Bool) -> List a -> List a
dropWhile pred items =
    case items of
        x :: xs ->
            if pred x then
                dropWhile pred xs
            else
                items

        _ ->
            items


normalizePath : String -> String -> String
normalizePath sep path =
    let
        sep1 =
            if sep == "/" then
                "\\"
            else
                "/"
    in
        path
            |> String.trim
            |> String.split sep1
            |> String.join sep


{-| Resolve a relative path start from a dir.
dir: absolute path
relativePath: a normalized relative path
return: absolute path
-}
resolvePath : String -> String -> String -> String
resolvePath sep dir path =
    if String.isEmpty dir || isAbsolutePath sep path then
        path
    else
        let
            --_ =
            --    Debug.log "dir" dir
            --_ =
            --    Debug.log "path" path
            parts =
                String.split sep path
                    |> dropWhile ((==) ".")

            n =
                parts
                    |> List.filter ((==) "..")
                    |> List.length

            dropEndPathSeperator s =
                if String.endsWith sep s then
                    String.dropRight (String.length sep) s
                else
                    s

            dirParts =
                dir
                    |> dropEndPathSeperator
                    |> String.split sep

            m =
                List.length dirParts
        in
            if m >= n then
                (List.take (m - n) dirParts ++ List.drop n parts)
                    |> String.join sep
            else
                parts
                    |> List.take (n - m)
                    |> String.join sep


{-| Calcuate relative path between two normalized absolute paths

    relativePath "/users/webvim/" "/users/webvim/src/main.elm" == "src/main.elm"

    relativePath "/a/b/" "/a/c/d.elm" == "../c/d.elm"

    relativePath "/a/c/d.elm" "/a/b/" == "../../b"

-}
relativePath : String -> String -> String -> String
relativePath sep from to =
    let
        commonAncestors a b ancestors =
            case a of
                x :: xs ->
                    case b of
                        y :: ys ->
                            if x == y then
                                commonAncestors xs ys (y :: ancestors)
                            else
                                ( a, b, ancestors )

                        _ ->
                            ( a, b, ancestors )

                _ ->
                    ( a, b, ancestors )

        ( fromParts, toParts, ancestors ) =
            commonAncestors
                (String.split sep from)
                (String.split sep to)
                []

        --|> Debug.log "result"
    in
        (List.repeat (List.length fromParts - 1) ".." ++ toParts)
            |> String.join sep


nthList : Int -> List a -> Maybe a
nthList i list =
    case list of
        x :: xs ->
            if i == 0 then
                Just x
            else
                nthList (i - 1) xs

        [] ->
            Nothing


arrayInsert : Int -> a -> Array a -> Array a
arrayInsert i item arr =
    (Array.slice i (Array.length arr) arr)
        |> Array.append (Array.fromList [ item ])
        |> Array.append (Array.slice 0 i arr)
