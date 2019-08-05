module Update.Keymap exposing (mapKeys)

import Dict
import Model exposing (Key)
import Model.Buffer exposing (..)
import Vim.Helper exposing (parseKeys)


{-| windows doesn't have cmd key, use alt key instead
-}
mapCmdToAlt : List ( String, String ) -> List ( String, String )
mapCmdToAlt k =
    k
        ++ List.filterMap
            (\( key, keys ) ->
                let
                    key1 =
                        String.replace "<m-" "<a-" key
                in
                if key1 == key then
                    Nothing

                else
                    Just ( key1, keys )
            )
            k


normalKeymap =
    mapCmdToAlt
        [ ( "<c-p>", ":o<space>" )
        , ( "<m-p>", ":o<space>" )
        , ( "<m-b>", ":b<space>" )
        , ( "<c-,>", ":f<space>" )
        , ( "<c-s>", ":w<enter>" )
        , ( "<m-s>", ":w<enter>" )
        , ( "<m-c>", ":copy<enter>" )
        , ( "<m-v>", "\"+P" )
        ]


insertKeymap =
    mapCmdToAlt
        [ ( "<m-v>", "<c-r>+" )
        ]


visualKeymap =
    mapCmdToAlt
        [ ( "<m-c>", "\"+y" )
        ]


mapKeys : Mode -> String -> List Key
mapKeys mode keys =
    keys
        |> parseKeys
        |> List.concatMap (keymap mode)


keymap : Mode -> Key -> List Key
keymap mode key =
    (case mode of
        Normal _ ->
            normalKeymap

        Insert _ ->
            insertKeymap

        Visual _ ->
            visualKeymap

        _ ->
            []
    )
        |> Dict.fromList
        |> Dict.get key
        |> Maybe.map parseKeys
        |> Maybe.withDefault [ key ]
