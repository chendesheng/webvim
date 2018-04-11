module Message exposing (..)

import Window exposing (Size)
import Result
import Http
import Position exposing (Position)


type alias Key =
    String


type alias BufferInfo =
    { path : String
    , cursor : Position
    , scrollTop : Int
    , content : Maybe String
    }


type Msg
    = PressKey Int Key -- buffer id, key
    | Resize Size
    | Read (Result Http.Error BufferInfo)
    | Write (Result Http.Error ())
    | Edit BufferInfo
