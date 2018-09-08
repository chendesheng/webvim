module Update.Keymap exposing (mapKeys)

import Vim.Helper exposing (parseKeys, escapeKey)
import Model exposing (Mode(..), Key)
import Dict


normalKeymap =
    [ ( "<c-p>", ":e<space>" )
    , ( "<c-,>", ":f<space>" )
    , ( "<c-s>", ":w<enter>" )
    , ( "<a-s>", ":w<enter>" )
    , ( "<m-s>", ":w<enter>" )
    , ( "<m-v>", "\"+P" )
    , ( "<a-v>", "\"+P" ) -- for windows
    ]


insertKeymap =
    [ ( "<m-v>", "<c-r>+" )
    , ( "<a-v>", "<c-r>+" )
    ]


visualKeymap =
    [ ( "<m-c>", "\"+y" )
    , ( "<a-c>", "\"+y" )
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
