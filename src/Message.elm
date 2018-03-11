module Message exposing (..)


type alias Key =
    String


type Msg
    = PressKey Int Key -- buffer id, key
