module Helper.KeyEvent exposing (decodeKeyboardEvent)

import Json.Decode exposing (Decoder, map, map5, field, andThen, maybe, succeed, fail, bool, string)
import Dict exposing (Dict)
import Helper.Helper exposing (isSingleChar, regex)
import Vim.Helper exposing (escapeKey)
import Regex as Re


decodeKeyboardEvent : Bool -> Decoder String
decodeKeyboardEvent replaceFullWidthToHalfWidth =
    map5 (toKey replaceFullWidthToHalfWidth)
        (field "ctrlKey" bool)
        (field "altKey" bool)
        (field "shiftKey" bool)
        (field "metaKey" bool)
        (field "key" string
            |> map
                (\key ->
                    if key == " " then
                        "Space"
                    else
                        case String.uncons key of
                            Just ( c, rest ) ->
                                if String.fromChar c == rest then
                                    rest
                                else
                                    key

                            _ ->
                                key
                )
        )
        |> andThen
            (\key ->
                if String.isEmpty key then
                    fail "empty key"
                else
                    succeed key
            )


shiftComboMap : Dict.Dict String String
shiftComboMap =
    Dict.fromList
        [ ( "s-1", "!" )
        , ( "s-2", "@" )
        , ( "s-3", "#" )
        , ( "s-4", "$" )
        , ( "s-5", "%" )
        , ( "s-6", "^" )
        , ( "s-7", "&" )
        , ( "s-8", "*" )
        , ( "s-9", "(" )
        , ( "s-0", ")" )
        , ( "s--", "_" )
        , ( "s-=", "+" )
        , ( "s-[", "{" )
        , ( "s-]", "}" )
        , ( "s-;", ":" )
        , ( "s-'", "\"" )
        , ( "s-,", "<" )
        , ( "s-.", ">" )
        , ( "s-`", "~" )
        , ( "s-\\", "|" )
        , ( "s-/", "?" )
        ]


toKey : Bool -> Bool -> Bool -> Bool -> Bool -> String -> String
toKey replaceFullWidthToHalfWidth ctrl alt shift meta key =
    if key == "Control" || key == "Shift" || key == "Meta" || key == "Alt" then
        ""
    else
        let
            key1 =
                if replaceFullWidthToHalfWidth then
                    fullWidthToHalfWidth key
                else
                    key

            key2 =
                if shift then
                    -- when Chinese IME, c-^ become c-s-6, convert to s-6 to ^
                    Dict.get ("s-" ++ key1) shiftComboMap
                        |> Maybe.withDefault key1
                else
                    key1

            singleChar =
                isSingleChar key2

            prefix =
                [ if meta then
                    "m-"
                  else
                    ""
                , if ctrl then
                    "c-"
                  else
                    ""
                , if alt then
                    "a-"
                  else
                    ""
                , if shift && not singleChar then
                    "s-"
                  else
                    ""
                ]
                    |> String.join ""
        in
            if singleChar && String.isEmpty prefix then
                escapeKey key2
            else
                "<"
                    ++ mapKey
                        (prefix
                            ++ if singleChar then
                                if not replaceFullWidthToHalfWidth then
                                    -- for combo keys, always replace full width to half width
                                    fullWidthToHalfWidth key2
                                else
                                    key2
                               else
                                String.toLower key2
                        )
                    ++ ">"


fullWidthToHalfWidthMap : Dict String String
fullWidthToHalfWidthMap =
    let
        fullWidths =
            String.toList "…；，。【】！（）—《》？：“「」～"

        halfWidths =
            String.toList "^;,.[]!()_<>?:\"{}~"
    in
        List.map2
            (\k v -> ( String.fromChar k, String.fromChar v ))
            fullWidths
            halfWidths
            |> Dict.fromList


fullWidthToHalfWidth : String -> String
fullWidthToHalfWidth s =
    fullWidthToHalfWidthMap
        |> Dict.get s
        |> Maybe.withDefault s


replaceFullWidthPunchation s =
    Re.replace (regex "")


keymap : Dict String String
keymap =
    Dict.fromList
        [ ( "c-[", "escape" )
        , ( "c-i", "tab" )
        , ( "c-m", "enter" )
        ]


mapKey : String -> String
mapKey key =
    keymap
        |> Dict.get key
        |> Maybe.withDefault key
