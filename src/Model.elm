module Model exposing (..)

import Message exposing (Msg(..))
import Position exposing (..)
import Internal.TextBuffer as B exposing (TextBuffer, Patch(..))
import Window as Win exposing (Size)
import Task
import Dict exposing (Dict)
import Vim.AST as V exposing (VisualType(..))
import Syntax exposing (..)
import Array


type alias Undo =
    { cursor : Position
    , patches : List Patch
    }


type alias Redo =
    Undo


type ExPrefix
    = ExSearch
        { forward : Bool
        , match : Maybe ( Position, Position ) -- increment cursor position
        }
    | ExCommand
    | ExEval


type alias VisualMode =
    { tipe : VisualType
    , begin : Position
    , end : Position
    }


type Mode
    = Normal
    | Visual VisualMode
    | Insert
    | TempNormal
    | Ex
        { prefix : ExPrefix
        , exbuf : Buffer
        , visual : Maybe VisualMode
        }


type alias View =
    { scrollTop : Int
    , scrollLeft : Int
    , startPosition : Position
    , dataStartPosition : Position
    , size : Size
    , statusbarHeight : Int

    -- pixel height of a line
    , lineHeight : Int
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
    , syntax : Syntax
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
    , registers : Dict String String
    , last :
        { matchChar :
            Maybe
                { char : String
                , before : Bool
                , forward : Bool
                }
        , matchString : Maybe ( String, Bool )
        , inserts : String
        , visual : String
        , ex : String
        , indent : Int
        }
    , vimASTCache : Dict ( String, String ) ( V.AST, String )
    }


cacheVimAST : ( String, String ) -> ( V.AST, String ) -> Buffer -> Buffer
cacheVimAST k v buf =
    { buf | vimASTCache = Dict.insert k v buf.vimASTCache }


emptyExBuffer : Buffer
emptyExBuffer =
    { emptyBuffer | mode = Insert, lines = B.empty }


emptyView : View
emptyView =
    { scrollTop = 0
    , scrollLeft = 0
    , startPosition = ( 0, 0 )
    , size = { width = 1, height = 1 }
    , dataStartPosition = ( 0, 0 )
    , statusbarHeight = 1
    , lineHeight = 21
    }


emptyBuffer : Buffer
emptyBuffer =
    { id = 0
    , lines = B.fromString B.lineBreak
    , syntax = { lang = "", lines = Array.empty }
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
    , view = emptyView
    , continuation = ""
    , registers = Dict.empty
    , last =
        { matchChar = Nothing
        , matchString = Nothing
        , inserts = ""
        , visual = ""
        , ex = ""
        , indent = 0
        }
    , vimASTCache = Dict.empty
    }


type alias Flags =
    { lineHeight : Int
    }


init : Flags -> ( Model, Cmd Msg )
init { lineHeight } =
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
                , view = { view | lineHeight = lineHeight }
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
