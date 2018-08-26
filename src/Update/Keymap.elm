module Update.Keymap exposing (keymap)

import Vim.Helper exposing (parseKeys, escapeKey)
import Model exposing (Mode(..), Key)
import Dict


normalKeyMap =
    [ ( "<c-p>", ":e<space>" )
    , ( "<c-,>", ":f<space>" )
    , ( "<c-s>", ":w<cr>" )
    , ( "<m-v>", "\"+P" )
    , ( "<a-v>", "\"+P" ) -- for windows
    ]


insertKeyMap =
    [ ( "<m-v>", "<c-r>+" )
    , ( "<a-v>", "<c-r>+" )
    ]


visualKeyMap =
    [ ( "<m-c>", "\"+y" )
    , ( "<a-c>", "\"+y" )
    ]


keymap : Mode -> Key -> List Key
keymap mode key =
    (case mode of
        Normal _ ->
            normalKeyMap

        Insert _ ->
            insertKeyMap

        Visual _ ->
            visualKeyMap

        _ ->
            []
    )
        |> Dict.fromList
        |> Dict.get key
        |> Maybe.map parseKeys
        |> Maybe.withDefault [ key ]
