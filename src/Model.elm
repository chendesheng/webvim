module Model exposing (..)

import Array as A
import Dict as D
import Message exposing (Msg(..))


type Position
    = Position Int Int


type alias TextBuffer =
    { lines : A.Array String
    , cursor : Position
    , path : String
    , name : String
    }


type alias Model =
    { buffers : D.Dict String TextBuffer
    , activeBuffer : TextBuffer
    }


emptyBuffer : TextBuffer
emptyBuffer =
    { lines = A.empty
    , cursor = Position 0 0
    , path = ""
    , name = "no name"
    }


init : ( Model, Cmd Msg )
init =
    ( { buffers = D.fromList [ ( emptyBuffer.path, emptyBuffer ) ]
      , activeBuffer = emptyBuffer
      }
    , Cmd.none
    )
