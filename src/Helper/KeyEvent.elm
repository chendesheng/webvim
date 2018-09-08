module Helper.KeyEvent exposing (decodeKeyboardEvent)

import Json.Decode exposing (Decoder, map, map5, field, andThen, maybe, succeed, fail, bool, string)
import Dict exposing (Dict)
import Helper.Helper exposing (isSingleChar, regex)
import Regex as Re


decodeKeyboardEvent : Decoder String
decodeKeyboardEvent =
    map5 toKey
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


shiftedKeys : String
shiftedKeys =
    "~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?"


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


toKey : Bool -> Bool -> Bool -> Bool -> String -> String
toKey ctrl alt shift meta key =
    if key == "Control" || key == "Shift" || key == "Meta" || key == "Alt" then
        ""
    else
        let
            _ =
                Debug.log "key" key

            key1 =
                fullWidthToHalfWidth key

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
                , if
                    shift
                        && (not singleChar
                                || (not <| String.contains key2 shiftedKeys)
                           )
                  then
                    "s-"
                  else
                    ""
                ]
                    |> String.join ""
        in
            if singleChar && String.isEmpty prefix then
                key2
            else
                "<"
                    ++ mapKey
                        (prefix
                            ++ if singleChar then
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
