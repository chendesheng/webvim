module Message exposing (..)

import Window exposing (Size)


type alias Key =
    String


type Msg
    = PressKey Int Key -- buffer id, key
    | Resize Size
