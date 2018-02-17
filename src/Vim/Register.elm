module Vim.Register exposing (..)

import Parser as P exposing ((|.), (|=), Parser)
import Vim.Helper exposing (..)
import Vim.State
    exposing
        ( Register
        , defaultRegister
        , Key
        , StateChange(..)
        , ModeDelta
        )
import Vim.Key exposing (keyParser)
import String


isRegister : Key -> Bool
isRegister key =
    let
        reg =
            key
                |> String.slice 0 1
                |> String.toList
                |> List.head
    in
        case reg of
            Just ch ->
                String.any
                    ((==) ch)
                    ("\"/=+_.@"
                        ++ "0123456789"
                        ++ "abcdefghijklmnopqrstuvwxyz"
                        ++ "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                    )

            Nothing ->
                False


registerParser : Parser ModeDelta
registerParser =
    readKeyAndThen "\""
        [ PushKey "\"" ]
        []
        (P.succeed
            (\key ->
                if isRegister key then
                    [ PushKeys [ "\"", key ], PushRegister key ]
                else
                    []
            )
            |= keyParser
        )


partialRegister : Register
partialRegister =
    ""
