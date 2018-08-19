module Update.Keymap exposing (keymap)

import Vim.Helper exposing (parseKeys)
import Model exposing (Mode(..), Key)


normalKeyMap : Key -> List Key
normalKeyMap key =
    (case key of
        "<c-p>" ->
            ":e<space>"

        "<c-,>" ->
            ":f "

        "<c-s>" ->
            ":w<cr>"

        _ ->
            ""
    )
        |> parseKeys
        |> Maybe.withDefault [ key ]


keymap : Mode -> Key -> List Key
keymap mode key =
    (case mode of
        Normal _ ->
            normalKeyMap

        _ ->
            List.singleton
    )
        key
