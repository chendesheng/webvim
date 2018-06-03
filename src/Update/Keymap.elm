module Update.Keymap exposing (keymap)

import Update.Message exposing (Key)
import Vim.Helper exposing (parseKeys)
import Model exposing (Mode(..))


normalKeyMap : Key -> List Key
normalKeyMap key =
    (case key of
        "<c-p>" ->
            ":e "

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
        Normal ->
            normalKeyMap

        _ ->
            List.singleton
    )
        key
