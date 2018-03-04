module Model exposing (..)

import Dict as D exposing (Dict)
import Message exposing (Msg(..))
import Types exposing (..)
import Internal.TextBuffer as B exposing (TextBuffer)


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


type alias View =
    { buffer : String -- buffer id
    , scrollTop : Int
    , startPosition : Position
    , lines : TextBuffer
    , cursor : Maybe Position
    , statusBar :
        { text : String
        , cmds : String
        , cursor : Maybe Int
        }
    , continuation : String
    }


type alias Model =
    { buffers : Dict String Buffer
    , activeBuffer : Buffer
    , view : Maybe View
    }


type alias BufferHistory =
    { undoes : List Undo
    , pending : Maybe Undo
    , redoes : List Redo
    }


type alias Buffer =
    { lines : TextBuffer
    , cursor : Position
    , cursorColumn : Int
    , path : String
    , name : String
    , mode : Mode
    , history : BufferHistory
    , config :
        { wordChars : String
        , tabSize : Int
        , expandTab : Bool
        }
    }


updateActiveBuffer : (Buffer -> Buffer) -> Model -> Model
updateActiveBuffer op model =
    { model | activeBuffer = op model.activeBuffer }


emptyBuffer : Buffer
emptyBuffer =
    { lines = B.empty
    , cursor = ( 0, 0 )
    , cursorColumn = 0
    , path = ""
    , name = "no name"
    , mode = Normal
    , history =
        { undoes = []
        , pending = Nothing
        , redoes = []
        }
    , config =
        { wordChars = "_" -- a-z and A-Z are word chars by default
        , tabSize = 4
        , expandTab = True
        }
    }


init : ( Model, Cmd Msg )
init =
    ( { buffers = D.fromList []
      , activeBuffer = emptyBuffer
      , view = Nothing
      }
    , Cmd.none
    )
