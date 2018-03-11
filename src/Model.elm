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
    | TempNormal
    | Ex
        { prefix : String
        , buffer : String
        , cursor : Position
        }


type alias View =
    { bufid : Int
    , scrollTop : Int
    , startPosition : Position
    , lines : TextBuffer
    , cursor : Position
    , statusBar :
        { text : String
        , cursor : Maybe Position
        }
    , height : Int
    , dataStartPosition : Position
    }


type alias Model =
    { buffers : Dict Int Buffer
    , maxId : Int
    , view : View
    }


type alias BufferHistory =
    { undoes : List Undo
    , pending : Maybe Undo
    , redoes : List Redo
    }


emptyBufferHistory : BufferHistory
emptyBufferHistory =
    { undoes = []
    , pending = Nothing
    , redoes = []
    }


type alias Buffer =
    { id : Int
    , lines : TextBuffer
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
    , view : View
    , continuation : String
    }



--updateActiveBuffer : (Buffer -> Buffer) -> Model -> Model
--updateActiveBuffer op model =
--    { model | activeBuffer = op model.activeBuffer }


getBuffer : Int -> Model -> Maybe Buffer
getBuffer bufid model =
    D.get bufid model.buffers


setBuffer : Int -> Buffer -> Model -> Model
setBuffer bufid buf model =
    { model | buffers = D.insert bufid buf model.buffers }


emptyBuffer : Buffer
emptyBuffer =
    { id = 0
    , lines = B.empty
    , cursor = ( 0, 0 )
    , cursorColumn = 0
    , path = ""
    , name = "no name"
    , mode = Normal
    , history = emptyBufferHistory
    , config =
        { wordChars = "_" -- a-z and A-Z are word chars by default
        , tabSize = 4
        , expandTab = True
        }
    , view =
        { bufid = 0
        , scrollTop = 0
        , startPosition = ( 0, 0 )
        , lines = B.empty
        , cursor = ( 0, 0 )
        , statusBar =
            { text = "-- Normal --"
            , cursor = Nothing
            }
        , height = 20
        , dataStartPosition = ( 0, 0 )
        }
    , continuation = ""
    }


getStatusBar : Mode -> { text : String, cursor : Maybe Position }
getStatusBar mode =
    case mode of
        Normal ->
            { text = "-- Normal --"
            , cursor = Nothing
            }

        Visual _ _ ->
            { text = "-- Visual --"
            , cursor = Nothing
            }

        Insert ->
            { text = "-- Insert --"
            , cursor = Nothing
            }

        TempNormal ->
            { text = "-- (Insert) --"
            , cursor = Nothing
            }

        Ex { prefix, buffer, cursor } ->
            { text = prefix ++ buffer
            , cursor = Just cursor
            }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        view =
            emptyBuffer.view

        lines =
            B.empty
                |> B.applyPatch (Insertion ( 0, 0 ) "1  23\n456")
                |> Tuple.second

        -- Ex { prefix = "/", buffer = "hello", cursor = ( 0, 0 ) }
        mode =
            Normal

        buf =
            { emptyBuffer
                | lines = lines
                , cursor = ( 0, 0 )
                , mode = mode
                , view =
                    { view
                        | lines = lines
                        , cursor = ( 0, 1 )
                        , statusBar = getStatusBar mode
                    }
            }
    in
        ( { buffers = D.fromList [ ( 0, buf ) ]
          , maxId = 0
          , view = buf.view
          }
        , Cmd.none
        )
