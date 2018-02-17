module Vim.Stack exposing (..)

import Vim.State exposing (Motion, ModeName, Operator)


type StateChange
    = Count Int
    | Register String
    | Operate Operator
    | PopKey
    | PushKey String
    | PushMode ModeName
    | PopMode
