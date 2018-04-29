port module Jumps
    exposing
        ( saveCursorPosition
        , onJump
        , jump
        )

import Position exposing (Position)


port saveCursorPosition : Position -> Cmd msg


port onJump : (Position -> msg) -> Sub msg


port jump : Bool -> Cmd msg
