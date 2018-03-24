module Model exposing (..)

import Message exposing (Msg(..))
import Position exposing (..)
import Internal.TextBuffer as B exposing (TextBuffer, Patch(..))
import Window as Win exposing (Size)
import Task


type alias Undo =
    { cursor : Position
    , patches : List Patch
    }


type alias Redo =
    Undo


type VisualType
    = VisualLine
    | VisualBlock
    | VisualRange


type Mode
    = Normal
    | Visual VisualType (List ( Position, Position ))
    | Insert
    | TempNormal
    | Ex String Buffer


type alias View =
    { scrollTop : Int
    , scrollLeft : Int
    , startPosition : Position
    , dataStartPosition : Position
    , size : Size
    , statusbarHeight : Int
    }


type alias Model =
    Buffer


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


emptyExBuffer : Buffer
emptyExBuffer =
    { emptyBuffer | mode = Insert, lines = B.empty }


emptyBuffer : Buffer
emptyBuffer =
    { id = 0
    , lines = B.fromString B.lineBreak
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
        { scrollTop = 0
        , scrollLeft = 0
        , startPosition = ( 0, 0 )
        , size = { width = 1, height = 1 }
        , dataStartPosition = ( 0, 0 )
        , statusbarHeight = 1
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

        Ex prefix buffer ->
            { text = B.toString buffer.lines
            , cursor = Just buffer.cursor
            }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        view =
            emptyBuffer.view

        lines =
            B.empty
                |> B.applyPatch
                    ("1  23\n456\n"
                        ++ String.repeat 50 "aa"
                        ++ "\n1dsafjdo  23\n456\n"
                        ++ "\n1dsafjdo  23\n456\n"
                        ++ "\n1dsafjdo  23\n456\n"
                        ++ "\n1dsafjdo  23\n456\n"
                        ++ "\n1dsafjdo  23\n456\n"
                        ++ "\n1dsafjdo  23\n456\n"
                        ++ "\n"
                        |> B.fromString
                        |> Insertion ( 0, 0 )
                    )
                |> Tuple.second

        -- Ex { prefix = "/", buffer = "hello", cursor = ( 0, 0 ) }
        mode =
            Normal

        buf =
            { emptyBuffer
                | lines = lines
                , cursor = ( 0, 1 )
                , cursorColumn = 1
                , mode = mode
            }
    in
        ( buf
        , Task.perform Resize Win.size
        )


updateView : (View -> View) -> Buffer -> Buffer
updateView f buf =
    let
        view =
            buf.view
    in
        { buf | view = f buf.view }
