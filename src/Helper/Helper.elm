module Helper.Helper exposing (..)

import Dict exposing (Dict)
import Regex as Re exposing (Regex)
import Char
import Parser as P exposing ((|.), (|=))
import Array as Array exposing (Array)


getLast : List a -> Maybe a
getLast list =
    case list of
        [] ->
            Nothing

        [ item ] ->
            Just item

        _ :: rest ->
            getLast rest


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
        s
            |> Re.split (regex "[/\\\\]")
            |> getLast
            |> Maybe.map (String.split "." >> List.reverse)
            |> Maybe.withDefault []
    of
        [ x ] ->
            ( x, "" )

        x :: xs ->
            ( xs
                |> List.reverse
                |> String.join "."
            , "." ++ x
            )

        _ ->
            ( "", "" )


findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex =
    let
        findIndexHelper i pred list =
            case list of
                [] ->
                    Nothing

                first :: rest ->
                    if pred first then
                        Just i
                    else
                        findIndexHelper (i + 1) pred rest
    in
        findIndexHelper 0


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
    Re.fromString s


isSpace : Char -> Bool
isSpace c =
    let
        code =
            Char.toCode c
    in
        code < 20 || code == 32


notSpace : Char -> Bool
notSpace c =
    let
        code =
            Char.toCode c
    in
        code >= 20 && code /= 32


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


oneOrMore : (Char -> Bool) -> P.Parser ()
oneOrMore pred =
    P.chompIf pred
        |. P.chompWhile pred


keepOneOrMore : (Char -> Bool) -> P.Parser String
keepOneOrMore pred =
    P.getChompedString (oneOrMore pred)


parseWords : String -> String -> List String
parseWords wordChars str =
    let
        isWordChars =
            word wordChars
    in
        str
            |> P.run
                (P.loop []
                    (\words ->
                        P.oneOf
                            [ P.succeed (P.Loop words)
                                |. oneOrMore (not << isWordChars)
                            , P.succeed
                                (\s ->
                                    P.Loop
                                        (if String.length s >= 2 then
                                            s :: words
                                         else
                                            words
                                        )
                                )
                                |= keepOneOrMore isWordChars
                            , P.succeed (P.Done words)
                                |. P.end
                            ]
                    )
                )
            |> Result.withDefault []


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
        Re.contains (regex "^[a-zA-Z]:\\\\")


joinPath : String -> String -> String -> String
joinPath sep a b =
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


regex : String -> Re.Regex
regex s =
    if s == "" then
        Re.never
    else
        s
            |> Re.fromString
            |> Maybe.withDefault Re.never


regexWith : Re.Options -> String -> Maybe Re.Regex
regexWith option s =
    if s == "" then
        Nothing
    else
        Re.fromStringWith option s


escapeRegex : String -> String
escapeRegex =
    let
        re =
            regexWith
                { caseInsensitive = False
                , multiline = True
                }
                "[-\\/\\\\^$*+?.()|[\\]{}]"
                |> Maybe.withDefault Re.never
    in
        Re.replace re (\{ match } -> "\\" ++ match)


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
            |> Re.replace (regex "[\\\\/]+") (always sep)
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

        ( fromParts, toParts, _ ) =
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


rightChar : String -> Maybe Char
rightChar =
    String.right 1
        >> String.uncons
        >> Maybe.map Tuple.first


swapCase : Char -> Char
swapCase ch =
    if Char.isUpper ch then
        Char.toLower ch
    else if Char.isLower ch then
        Char.toUpper ch
    else
        ch


floorFromZero : Float -> Int
floorFromZero n =
    if n < 0 then
        ceiling n
    else
        floor n
