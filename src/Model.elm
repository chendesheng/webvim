module Model exposing (..)

import Array as A exposing (Array)
import Dict as D exposing (Dict)
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


type alias View =
    { buffer : String -- buffer id
    , scrollTop : Int
    , lineTop : Int
    , lines : List String
    , cursor : Maybe Position
    , statusBar :
        { text : String
        , cmds : String
        , cursor : Maybe Int
        }
    }


type alias Client =
    { views : List View
    }


type alias Model =
    { buffers : Dict String Buffer
    , activeBuffer : Buffer
    , clients : Dict String Client
    }


type alias Buffer =
    { lines : TextBuffer
    , cursor : Position
    , cursorColumn : Int
    , path : String
    , name : String
    , mode : Mode
    , continuation : String
    , history : ( List Undo, List Redo )
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
    { lines = A.empty
    , cursor = ( 0, 0 )
    , cursorColumn = 0
    , path = ""
    , name = "no name"
    , mode = Normal
    , continuation = ""
    , history = ( [], [] )
    , config =
        { wordChars = "_" -- a-z and A-Z are word chars by default
        , tabSize = 4
        , expandTab = True
        }
    }


init : ( Model, Cmd Msg )
init =
    ( { buffers = D.fromList [ ( emptyBuffer.path, emptyBuffer ) ]
      , activeBuffer = emptyBuffer
      , clients = D.empty
      }
    , Cmd.none
    )
