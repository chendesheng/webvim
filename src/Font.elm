module Font exposing (FontInfo, charWidth, cursorCharWidth, stringWidth)

import Dict
import Helper.Helper exposing (findFirst, regex)
import Regex as Re


type alias FontInfo =
    { name : String
    , widths : List ( String, Float )
    , lineHeight : Int
    , size : Int -- pt
    }


stringWidth : FontInfo -> Int -> Int -> String -> Int
stringWidth fontInfo b e s =
    s
        |> String.slice b e
        |> String.toList
        |> List.map (charWidth fontInfo)
        |> List.sum
        |> round


cursorCharWidth : FontInfo -> Int -> String -> Int
cursorCharWidth fontInfo x s =
    s
        |> String.dropLeft x
        |> String.uncons
        |> Maybe.map (Tuple.first >> charWidth fontInfo)
        |> Maybe.withDefault (charWidth fontInfo '0')
        |> round


charWidth : FontInfo -> Char -> Float
charWidth { widths } ch =
    let
        dict =
            Dict.fromList widths

        codePoint =
            Char.toCode ch

        widthType =
            charWidthType ch
    in
    widths
        |> findFirst
            (\( tipe, width ) ->
                tipe == widthType
            )
        |> Maybe.map Tuple.second
        |> Maybe.withDefault 10


{-| copy from: <https://github.com/Microsoft/vscode/blob/3a619f24c3b7f760f283193ebd9c3ed601768a83/src/vs/base/common/strings.ts>
-}
reEmoji =
    regex "(?:[\\u231A\\u231B\\u23F0\\u23F3\\u2600-\\u27BF\\u2B50\\u2B55]|\\uD83C[\\uDDE6-\\uDDFF\\uDF00-\\uDFFF]|\\uD83D[\\uDC00-\\uDE4F\\uDE80-\\uDEF8]|\\uD83E[\\uDD00-\\uDDE6])"


charWidthType : Char -> String
charWidthType char =
    let
        codePoint =
            Char.toCode char
    in
    -- https://github.com/Microsoft/vscode/blob/3a619f24c3b7f760f283193ebd9c3ed601768a83/src/vs/base/common/strings.ts#L535
    if
        (codePoint >= 0x2E80 && codePoint <= 0xD7AF)
            || (codePoint >= 0xF900 && codePoint <= 0xFAFF)
            || (codePoint >= 0xFF01 && codePoint <= 0xFF5E)
    then
        "FULL"

    else if Re.contains reEmoji <| String.fromChar char then
        "EMOJI"

    else
        "HALF"
