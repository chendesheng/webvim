module Helper.KeyEvent exposing (decodeKeyboardEvent)

import Json.Decode exposing (Decoder, map, map7, int, field, oneOf, andThen, maybe, succeed, fail, bool, string)
import String
import Dict


{-| A type alias for `Int`.
-}
type alias KeyCode =
    Int


{-| Decodes `keyCode`, `which` or `charCode` from a [keyboard event][keyboard-event]
to get a numeric code for the key that was pressed.

[keyboard-event]: https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent

-}
decodeKeyCode : Decoder KeyCode
decodeKeyCode =
    oneOf
        [ field "keyCode" decodeNonZero
        , field "which" decodeNonZero
        , field "charCode" decodeNonZero

        -- In principle, we should always get some code, so instead
        -- of making this a Maybe, we succeed with 0.
        , succeed 0
        ]


{-| Decodes an Int, but only if it's not zero.
-}
decodeNonZero : Decoder Int
decodeNonZero =
    andThen
        (\code ->
            if code == 0 then
                fail "code was zero"
            else
                succeed code
        )
        int


{-| Decodes the `key` field from a [keyboard event][keyboard-event].
Results in `Nothing` if the `key` field is not present, or blank.

[keyboard-event]: https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent

-}
decodeKey : Decoder (Maybe String)
decodeKey =
    field "key" string
        |> andThen
            (\key ->
                if String.isEmpty key then
                    fail "empty key"
                else
                    succeed key
            )
        |> maybe


{-| A representation of a [keyboard event][keyboard-event].

The `key` field may or may not be present, depending on the listener ("keydown"
vs. "keypress" vs. "keyup"), browser, and key pressed (character key vs.
special key). If not present, it will be `Nothing` here.

The `keyCode` is normalized by `decodeKeyboardEvent` to use whichever of
`which`, `keyCode` or `charCode` is provided, and made type-safe via
`Keyboard.Key`
(see the excellent [SwiftsNamesake/proper-keyboard][proper-keyboard-pkg] for
further manipulation of a `Key`).

[keyboard-event]: https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent
[proper-keyboard-pkg]: http://package.elm-lang.org/packages/SwiftsNamesake/proper-keyboard/latest

-}
type alias KeyboardEvent =
    { altKey : Bool
    , ctrlKey : Bool
    , key : Maybe String
    , keyCode : KeyCode
    , metaKey : Bool
    , repeat : Bool
    , shiftKey : Bool
    }


{-| Decodes a `KeyboardEvent` from a [keyboard event][keyboard-event].

[keyboard-event]: https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent

-}
decodeKeyboardEvent : Decoder String
decodeKeyboardEvent =
    map7 KeyboardEvent
        (field "altKey" bool)
        (field "ctrlKey" bool)
        decodeKey
        decodeKeyCode
        (field "metaKey" bool)
        (field "repeat" bool)
        (field "shiftKey" bool)
        |> andThen
            (\e ->
                case toKey e of
                    "" ->
                        fail "empty key"

                    key ->
                        succeed key
            )


keycodeMap : Dict.Dict Int String
keycodeMap =
    Dict.fromList
        [ ( 8, "backspace" )
        , ( 9, "tab" )
        , ( 12, "num" )
        , ( 13, "cr" )
        , ( 16, "shift" )
        , ( 17, "ctrl" )
        , ( 18, "alt" )
        , ( 19, "pause" )
        , ( 20, "caps" )
        , ( 27, "esc" )
        , ( 32, "space" )
        , ( 33, "pageup" )
        , ( 34, "pagedown" )
        , ( 35, "end" )
        , ( 36, "home" )
        , ( 37, "left" )
        , ( 38, "up" )
        , ( 39, "right" )
        , ( 40, "down" )
        , ( 44, "print" )
        , ( 45, "insert" )
        , ( 46, "delete" )
        , ( 48, "0" )
        , ( 49, "1" )
        , ( 50, "2" )
        , ( 51, "3" )
        , ( 52, "4" )
        , ( 53, "5" )
        , ( 54, "6" )
        , ( 55, "7" )
        , ( 56, "8" )
        , ( 57, "9" )
        , ( 65, "a" )
        , ( 66, "b" )
        , ( 67, "c" )
        , ( 68, "d" )
        , ( 69, "e" )
        , ( 70, "f" )
        , ( 71, "g" )
        , ( 72, "h" )
        , ( 73, "i" )
        , ( 74, "j" )
        , ( 75, "k" )
        , ( 76, "l" )
        , ( 77, "m" )
        , ( 78, "n" )
        , ( 79, "o" )
        , ( 80, "p" )
        , ( 81, "q" )
        , ( 82, "r" )
        , ( 83, "s" )
        , ( 84, "t" )
        , ( 85, "u" )
        , ( 86, "v" )
        , ( 87, "w" )
        , ( 88, "x" )
        , ( 89, "y" )
        , ( 90, "z" )
        , ( 91, "cmd" )
        , ( 92, "cmd" )
        , ( 93, "cmd" )
        , ( 96, "num_0" )
        , ( 97, "num_1" )
        , ( 98, "num_2" )
        , ( 99, "num_3" )
        , ( 100, "num_4" )
        , ( 101, "num_5" )
        , ( 102, "num_6" )
        , ( 103, "num_7" )
        , ( 104, "num_8" )
        , ( 105, "num_9" )
        , ( 106, "num_multiply" )
        , ( 107, "num_add" )
        , ( 108, "num_enter" )
        , ( 109, "num_subtract" )
        , ( 110, "num_decimal" )
        , ( 111, "num_divide" )
        , ( 112, "f1" )
        , ( 113, "f2" )
        , ( 114, "f3" )
        , ( 115, "f4" )
        , ( 116, "f5" )
        , ( 117, "f6" )
        , ( 118, "f7" )
        , ( 119, "f8" )
        , ( 120, "f9" )
        , ( 121, "f10" )
        , ( 122, "f11" )
        , ( 123, "f12" )
        , ( 124, "print" )
        , ( 144, "num" )
        , ( 145, "scroll" )
        , ( 186, ";" )
        , ( 187, "=" )
        , ( 188, "," )
        , ( 189, "-" )
        , ( 190, "." )
        , ( 191, "/" )
        , ( 192, "`" )
        , ( 219, "[" )
        , ( 220, "\\" )
        , ( 221, "]" )
        , ( 222, "'" )
        , ( 223, "`" )
        , ( 224, "cmd" )
        , ( 225, "alt" )
        , ( 57392, "ctrl" )
        , ( 63289, "num" )
        , ( 59, ";" )
        , ( 61, "=" )
        , ( 173, "-" )
        ]


mapKeyCode : Int -> String
mapKeyCode keyCode =
    keycodeMap
        |> Dict.get keyCode
        |> Maybe.withDefault ""


mapCtrl : String -> String
mapCtrl key =
    Dict.fromList
        [ ( "c-[", "esc" )
        , ( "c-i", "tab" )
        ]
        |> Dict.get key
        |> Maybe.withDefault key


shiftComboMap : Dict.Dict String String
shiftComboMap =
    Dict.fromList
        [ ( "s-a", "A" )
        , ( "s-b", "B" )
        , ( "s-c", "C" )
        , ( "s-d", "D" )
        , ( "s-e", "E" )
        , ( "s-f", "F" )
        , ( "s-g", "G" )
        , ( "s-h", "H" )
        , ( "s-i", "I" )
        , ( "s-j", "J" )
        , ( "s-k", "K" )
        , ( "s-l", "L" )
        , ( "s-m", "M" )
        , ( "s-n", "N" )
        , ( "s-o", "O" )
        , ( "s-p", "P" )
        , ( "s-q", "Q" )
        , ( "s-r", "R" )
        , ( "s-s", "S" )
        , ( "s-t", "T" )
        , ( "s-u", "U" )
        , ( "s-v", "V" )
        , ( "s-w", "W" )
        , ( "s-x", "X" )
        , ( "s-y", "Y" )
        , ( "s-z", "Z" )
        , ( "s-1", "!" )
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


mapShift : String -> String
mapShift key =
    shiftComboMap
        |> Dict.get key
        |> Maybe.withDefault key


toKey : KeyboardEvent -> String
toKey { ctrlKey, altKey, shiftKey, keyCode, metaKey } =
    let
        key =
            mapKeyCode keyCode

        quote k =
            if String.length k > 1 then
                "<" ++ k ++ ">"
            else
                k
    in
        (if
            (keyCode /= 0)
                && (keyCode /= 16)
                && (keyCode /= 17)
                && (keyCode /= 18)
                && (keyCode /= 93)
         then
            let
                key1 =
                    if shiftKey then
                        mapShift ("s-" ++ key)
                    else
                        key

                key2 =
                    if ctrlKey then
                        mapCtrl ("c-" ++ key1)
                    else if altKey then
                        "a-" ++ key1
                    else
                        key1
            in
                if metaKey then
                    "m-" ++ key2
                else
                    key2
         else
            ""
        )
            |> quote
