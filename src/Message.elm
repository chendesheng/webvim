module Message exposing (..)

import Window exposing (Size)
import Result
import Http


type alias Key =
    String


type Msg
    = PressKey Int Key -- buffer id, key
    | Resize Size
    | Read
        (Result Http.Error
            { path : String
            , content : String
            }
        )
    | Write (Result Http.Error ())
