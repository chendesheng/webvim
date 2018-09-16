module Helper.Helper exposing (..)

import Dict exposing (Dict)
import Regex as Re exposing (Regex)
import Char
import Parser as P exposing ((|.), (|=), Parser)
import Array as Array exposing (Array)


repeatParser : Parser a -> Parser (List a)
repeatParser parser =
    P.loop [] <|
        (\items ->
            P.oneOf
                [ P.succeed (\item -> P.Loop (item :: items))
                    |= parser
                , P.succeed (P.Done <| List.reverse items)
                ]
        )


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


findFirst : (a -> Bool) -> List a -> Maybe a
findFirst pred list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if pred first then
                Just first
            else
                findFirst pred rest


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


keepZeroOrMore : (Char -> Bool) -> P.Parser String
keepZeroOrMore pred =
    P.getChompedString <|
        P.chompWhile pred


chompUntilAfter : String -> P.Parser ()
chompUntilAfter s =
    P.chompUntil s |. P.symbol s


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


inc : number -> number
inc i =
    i + 1


dec : number -> number
dec i =
    i + 1


spaceInline : Char -> Bool
spaceInline char =
    isSpace char && char /= '\n'


charAt : Int -> String -> Maybe Char
charAt x s =
    String.dropLeft x s
        |> String.uncons
        |> Maybe.map Tuple.first


isSingleChar : String -> Bool
isSingleChar s =
    case String.uncons s of
        Just ( first, rest ) ->
            String.isEmpty rest

        _ ->
            False


{-| copy from: <https://github.com/Microsoft/vscode/blob/3a619f24c3b7f760f283193ebd9c3ed601768a83/src/vs/base/common/strings.ts>
-}
reEmoji =
    regex "(?:[\\u231A\\u231B\\u23F0\\u23F3\\u2600-\\u27BF\\u2B50\\u2B55]|\\uD83C[\\uDDE6-\\uDDFF\\uDF00-\\uDFFF]|\\uD83D[\\uDC00-\\uDE4F\\uDE80-\\uDEF8]|\\uD83E[\\uDD00-\\uDDE6])"


charWidthType : Char -> String
charWidthType ch =
    let
        codePoint =
            Char.toCode ch
    in
        if
            ((0x3000 == codePoint)
                || (0xFF01 <= codePoint && codePoint <= 0xFF60)
                || (0xFFE0 <= codePoint && codePoint <= 0xFFE6)
            )
        then
            "F"
        else if
            ((0x20A9 == codePoint)
                || (0xFF61 <= codePoint && codePoint <= 0xFFBE)
                || (0xFFC2 <= codePoint && codePoint <= 0xFFC7)
                || (0xFFCA <= codePoint && codePoint <= 0xFFCF)
                || (0xFFD2 <= codePoint && codePoint <= 0xFFD7)
                || (0xFFDA <= codePoint && codePoint <= 0xFFDC)
                || (0xFFE8 <= codePoint && codePoint <= 0xFFEE)
            )
        then
            "H"
        else if
            ((0x1100 <= codePoint && codePoint <= 0x115F)
                || (0x11A3 <= codePoint && codePoint <= 0x11A7)
                || (0x11FA <= codePoint && codePoint <= 0x11FF)
                || (0x2329 <= codePoint && codePoint <= 0x232A)
                || (0x2E80 <= codePoint && codePoint <= 0x2E99)
                || (0x2E9B <= codePoint && codePoint <= 0x2EF3)
                || (0x2F00 <= codePoint && codePoint <= 0x2FD5)
                || (0x2FF0 <= codePoint && codePoint <= 0x2FFB)
                || (0x3001 <= codePoint && codePoint <= 0x303E)
                || (0x3041 <= codePoint && codePoint <= 0x3096)
                || (0x3099 <= codePoint && codePoint <= 0x30FF)
                || (0x3105 <= codePoint && codePoint <= 0x312D)
                || (0x3131 <= codePoint && codePoint <= 0x318E)
                || (0x3190 <= codePoint && codePoint <= 0x31BA)
                || (0x31C0 <= codePoint && codePoint <= 0x31E3)
                || (0x31F0 <= codePoint && codePoint <= 0x321E)
                || (0x3220 <= codePoint && codePoint <= 0x3247)
                || (0x3250 <= codePoint && codePoint <= 0x32FE)
                || (0x3300 <= codePoint && codePoint <= 0x4DBF)
                || (0x4E00 <= codePoint && codePoint <= 0xA48C)
                || (0xA490 <= codePoint && codePoint <= 0xA4C6)
                || (0xA960 <= codePoint && codePoint <= 0xA97C)
                || (0xAC00 <= codePoint && codePoint <= 0xD7A3)
                || (0xD7B0 <= codePoint && codePoint <= 0xD7C6)
                || (0xD7CB <= codePoint && codePoint <= 0xD7FB)
                || (0xF900 <= codePoint && codePoint <= 0xFAFF)
                || (0xFE10 <= codePoint && codePoint <= 0xFE19)
                || (0xFE30 <= codePoint && codePoint <= 0xFE52)
                || (0xFE54 <= codePoint && codePoint <= 0xFE66)
                || (0xFE68 <= codePoint && codePoint <= 0xFE6B)
                || (0x0001B000 <= codePoint && codePoint <= 0x0001B001)
                || (0x0001F200 <= codePoint && codePoint <= 0x0001F202)
                || (0x0001F210 <= codePoint && codePoint <= 0x0001F23A)
                || (0x0001F240 <= codePoint && codePoint <= 0x0001F248)
                || (0x0001F250 <= codePoint && codePoint <= 0x0001F251)
                || (0x00020000 <= codePoint && codePoint <= 0x0002F73F)
                || (0x0002B740 <= codePoint && codePoint <= 0x0002FFFD)
                || (0x00030000 <= codePoint && codePoint <= 0x0003FFFD)
            )
        then
            "W"
        else if
            ((0x20 <= codePoint && codePoint <= 0x7E)
                || (0xA2 <= codePoint && codePoint <= 0xA3)
                || (0xA5 <= codePoint && codePoint <= 0xA6)
                || (0xAC == codePoint)
                || (0xAF == codePoint)
                || (0x27E6 <= codePoint && codePoint <= 0x27ED)
                || (0x2985 <= codePoint && codePoint <= 0x2986)
            )
        then
            "Na"
        else if
            ((0xA1 == codePoint)
                || (0xA4 == codePoint)
                || (0xA7 <= codePoint && codePoint <= 0xA8)
                || (0xAA == codePoint)
                || (0xAD <= codePoint && codePoint <= 0xAE)
                || (0xB0 <= codePoint && codePoint <= 0xB4)
                || (0xB6 <= codePoint && codePoint <= 0xBA)
                || (0xBC <= codePoint && codePoint <= 0xBF)
                || (0xC6 == codePoint)
                || (0xD0 == codePoint)
                || (0xD7 <= codePoint && codePoint <= 0xD8)
                || (0xDE <= codePoint && codePoint <= 0xE1)
                || (0xE6 == codePoint)
                || (0xE8 <= codePoint && codePoint <= 0xEA)
                || (0xEC <= codePoint && codePoint <= 0xED)
                || (0xF0 == codePoint)
                || (0xF2 <= codePoint && codePoint <= 0xF3)
                || (0xF7 <= codePoint && codePoint <= 0xFA)
                || (0xFC == codePoint)
                || (0xFE == codePoint)
                || (0x0101 == codePoint)
                || (0x0111 == codePoint)
                || (0x0113 == codePoint)
                || (0x011B == codePoint)
                || (0x0126 <= codePoint && codePoint <= 0x0127)
                || (0x012B == codePoint)
                || (0x0131 <= codePoint && codePoint <= 0x0133)
                || (0x0138 == codePoint)
                || (0x013F <= codePoint && codePoint <= 0x0142)
                || (0x0144 == codePoint)
                || (0x0148 <= codePoint && codePoint <= 0x014B)
                || (0x014D == codePoint)
                || (0x0152 <= codePoint && codePoint <= 0x0153)
                || (0x0166 <= codePoint && codePoint <= 0x0167)
                || (0x016B == codePoint)
                || (0x01CE == codePoint)
                || (0x01D0 == codePoint)
                || (0x01D2 == codePoint)
                || (0x01D4 == codePoint)
                || (0x01D6 == codePoint)
                || (0x01D8 == codePoint)
                || (0x01DA == codePoint)
                || (0x01DC == codePoint)
                || (0x0251 == codePoint)
                || (0x0261 == codePoint)
                || (0x02C4 == codePoint)
                || (0x02C7 == codePoint)
                || (0x02C9 <= codePoint && codePoint <= 0x02CB)
                || (0x02CD == codePoint)
                || (0x02D0 == codePoint)
                || (0x02D8 <= codePoint && codePoint <= 0x02DB)
                || (0x02DD == codePoint)
                || (0x02DF == codePoint)
                || (0x0300 <= codePoint && codePoint <= 0x036F)
                || (0x0391 <= codePoint && codePoint <= 0x03A1)
                || (0x03A3 <= codePoint && codePoint <= 0x03A9)
                || (0x03B1 <= codePoint && codePoint <= 0x03C1)
                || (0x03C3 <= codePoint && codePoint <= 0x03C9)
                || (0x0401 == codePoint)
                || (0x0410 <= codePoint && codePoint <= 0x044F)
                || (0x0451 == codePoint)
                || (0x2010 == codePoint)
                || (0x2013 <= codePoint && codePoint <= 0x2016)
                || (0x2018 <= codePoint && codePoint <= 0x2019)
                || (0x201C <= codePoint && codePoint <= 0x201D)
                || (0x2020 <= codePoint && codePoint <= 0x2022)
                || (0x2024 <= codePoint && codePoint <= 0x2027)
                || (0x2030 == codePoint)
                || (0x2032 <= codePoint && codePoint <= 0x2033)
                || (0x2035 == codePoint)
                || (0x203B == codePoint)
                || (0x203E == codePoint)
                || (0x2074 == codePoint)
                || (0x207F == codePoint)
                || (0x2081 <= codePoint && codePoint <= 0x2084)
                || (0x20AC == codePoint)
                || (0x2103 == codePoint)
                || (0x2105 == codePoint)
                || (0x2109 == codePoint)
                || (0x2113 == codePoint)
                || (0x2116 == codePoint)
                || (0x2121 <= codePoint && codePoint <= 0x2122)
                || (0x2126 == codePoint)
                || (0x212B == codePoint)
                || (0x2153 <= codePoint && codePoint <= 0x2154)
                || (0x215B <= codePoint && codePoint <= 0x215E)
                || (0x2160 <= codePoint && codePoint <= 0x216B)
                || (0x2170 <= codePoint && codePoint <= 0x2179)
                || (0x2189 == codePoint)
                || (0x2190 <= codePoint && codePoint <= 0x2199)
                || (0x21B8 <= codePoint && codePoint <= 0x21B9)
                || (0x21D2 == codePoint)
                || (0x21D4 == codePoint)
                || (0x21E7 == codePoint)
                || (0x2200 == codePoint)
                || (0x2202 <= codePoint && codePoint <= 0x2203)
                || (0x2207 <= codePoint && codePoint <= 0x2208)
                || (0x220B == codePoint)
                || (0x220F == codePoint)
                || (0x2211 == codePoint)
                || (0x2215 == codePoint)
                || (0x221A == codePoint)
                || (0x221D <= codePoint && codePoint <= 0x2220)
                || (0x2223 == codePoint)
                || (0x2225 == codePoint)
                || (0x2227 <= codePoint && codePoint <= 0x222C)
                || (0x222E == codePoint)
                || (0x2234 <= codePoint && codePoint <= 0x2237)
                || (0x223C <= codePoint && codePoint <= 0x223D)
                || (0x2248 == codePoint)
                || (0x224C == codePoint)
                || (0x2252 == codePoint)
                || (0x2260 <= codePoint && codePoint <= 0x2261)
                || (0x2264 <= codePoint && codePoint <= 0x2267)
                || (0x226A <= codePoint && codePoint <= 0x226B)
                || (0x226E <= codePoint && codePoint <= 0x226F)
                || (0x2282 <= codePoint && codePoint <= 0x2283)
                || (0x2286 <= codePoint && codePoint <= 0x2287)
                || (0x2295 == codePoint)
                || (0x2299 == codePoint)
                || (0x22A5 == codePoint)
                || (0x22BF == codePoint)
                || (0x2312 == codePoint)
                || (0x2460 <= codePoint && codePoint <= 0x24E9)
                || (0x24EB <= codePoint && codePoint <= 0x254B)
                || (0x2550 <= codePoint && codePoint <= 0x2573)
                || (0x2580 <= codePoint && codePoint <= 0x258F)
                || (0x2592 <= codePoint && codePoint <= 0x2595)
                || (0x25A0 <= codePoint && codePoint <= 0x25A1)
                || (0x25A3 <= codePoint && codePoint <= 0x25A9)
                || (0x25B2 <= codePoint && codePoint <= 0x25B3)
                || (0x25B6 <= codePoint && codePoint <= 0x25B7)
                || (0x25BC <= codePoint && codePoint <= 0x25BD)
                || (0x25C0 <= codePoint && codePoint <= 0x25C1)
                || (0x25C6 <= codePoint && codePoint <= 0x25C8)
                || (0x25CB == codePoint)
                || (0x25CE <= codePoint && codePoint <= 0x25D1)
                || (0x25E2 <= codePoint && codePoint <= 0x25E5)
                || (0x25EF == codePoint)
                || (0x2605 <= codePoint && codePoint <= 0x2606)
                || (0x2609 == codePoint)
                || (0x260E <= codePoint && codePoint <= 0x260F)
                || (0x2614 <= codePoint && codePoint <= 0x2615)
                || (0x261C == codePoint)
                || (0x261E == codePoint)
                || (0x2640 == codePoint)
                || (0x2642 == codePoint)
                || (0x2660 <= codePoint && codePoint <= 0x2661)
                || (0x2663 <= codePoint && codePoint <= 0x2665)
                || (0x2667 <= codePoint && codePoint <= 0x266A)
                || (0x266C <= codePoint && codePoint <= 0x266D)
                || (0x266F == codePoint)
                || (0x269E <= codePoint && codePoint <= 0x269F)
                || (0x26BE <= codePoint && codePoint <= 0x26BF)
                || (0x26C4 <= codePoint && codePoint <= 0x26CD)
                || (0x26CF <= codePoint && codePoint <= 0x26E1)
                || (0x26E3 == codePoint)
                || (0x26E8 <= codePoint && codePoint <= 0x26FF)
                || (0x273D == codePoint)
                || (0x2757 == codePoint)
                || (0x2776 <= codePoint && codePoint <= 0x277F)
                || (0x2B55 <= codePoint && codePoint <= 0x2B59)
                || (0x3248 <= codePoint && codePoint <= 0x324F)
                || (0xE000 <= codePoint && codePoint <= 0xF8FF)
                || (0xFE00 <= codePoint && codePoint <= 0xFE0F)
                || (0xFFFD == codePoint)
                || (0x0001F100 <= codePoint && codePoint <= 0x0001F10A)
                || (0x0001F110 <= codePoint && codePoint <= 0x0001F12D)
                || (0x0001F130 <= codePoint && codePoint <= 0x0001F169)
                || (0x0001F170 <= codePoint && codePoint <= 0x0001F19A)
                || (0x000E0100 <= codePoint && codePoint <= 0x000E01EF)
                || (0x000F0000 <= codePoint && codePoint <= 0x000FFFFD)
                || (0x00100000 <= codePoint && codePoint <= 0x0010FFFD)
            )
        then
            "A"
        else if Re.contains reEmoji <| String.fromChar ch then
            "EMOJI"
        else
            "N"
