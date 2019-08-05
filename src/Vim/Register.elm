module Vim.Register exposing (isRegisterChar, registerKey, registerKeyEnd)

import Parser as P exposing ((|=), Parser)
import String
import Vim.AST exposing (Key, ModeDelta, StateChange(..))
import Vim.Helper exposing (..)


isRegisterChar : Char -> Bool
isRegisterChar ch =
    String.any
        ((==) ch)
        ("\"/=+_.@%#"
            ++ "0123456789"
            ++ "abcdefghijklmnopqrstuvwxyz"
            ++ "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        )


registerKeyEnd : (Key -> ModeDelta) -> Parser ModeDelta
registerKeyEnd =
    registerKey (P.succeed [])


registerKey : Parser ModeDelta -> (Key -> ModeDelta) -> Parser ModeDelta
registerKey next f =
    P.oneOf
        [ P.succeed f
            |= (P.chompIf isRegisterChar
                    |> P.getChompedString
               )
        , next
        ]
