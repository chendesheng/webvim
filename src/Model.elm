module Model exposing (..)

import Array as A exposing (Array)
import Dict as D
import Message exposing (Msg(..))
import Types exposing (..)
import Internal.TextBuffer exposing (TextBuffer)


type VisualType
    = VisualLine
    | VisualBlock
    | VisualRange


type Mode
    = Normal
    | Visual VisualType (List ( Position, Position ))
    | Insert
    | Ex
        { prefix : String
        , buffer : String
        , cursor : Position
        }


type alias Model =
    { buffers : D.Dict String Buffer
    , activeBuffer : Buffer
    }


type alias Buffer =
    { lines : TextBuffer
    , cursor : Position
    , path : String
    , name : String
    , mode : Mode
    , continuation : String
    , history : ( List Undo, List Redo )
    }


updateActiveBuffer : (Buffer -> Buffer) -> Model -> Model
updateActiveBuffer op model =
    { model | activeBuffer = op model.activeBuffer }


emptyBuffer : Buffer
emptyBuffer =
    { lines = A.empty
    , cursor = ( 0, 0 )
    , path = ""
    , name = "no name"
    , mode = Normal
    , continuation = ""
    , history = ( [], [] )
    }


init : ( Model, Cmd Msg )
init =
    ( { buffers = D.fromList [ ( emptyBuffer.path, emptyBuffer ) ]
      , activeBuffer = emptyBuffer
      }
    , Cmd.none
    )
