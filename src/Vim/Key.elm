module Vim.Key exposing (..)

import Parser as P exposing (Parser, (|.), (|=))
import Vim.Helper exposing (..)
import Vim.State exposing (Key)


escapedChar : Parser String
escapedChar =
    P.oneOf
        -- escape '<' and '\'
        [ P.succeed identity
            |. ignoreChar ((==) '\\')
            |= keepChar (\ch -> ch == '<' || ch == '\\')
        , keepChar (\ch -> ch /= '<')
        ]


keyParser : Parser Key
keyParser =
    P.oneOf
        [ escapedChar
        , (P.symbol "<" |. P.ignoreUntil ">")
            |> P.source
        ]


popKey : String -> String
popKey keys =
    let
        lastKey =
            P.run (P.repeat P.zeroOrMore keyParser) keys
                |> Result.map (getLast >> Maybe.withDefault "")
                |> Result.withDefault ""
                |> escapeKey
    in
        String.dropRight (String.length lastKey) keys


popKeys : Int -> String -> String
popKeys n keys =
    if n == 0 then
        keys
    else
        popKeys (n - 1) (popKey keys)
