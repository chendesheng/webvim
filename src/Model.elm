module Model exposing (..)

import Message exposing (Msg(..), BufferInfo, LocationItem)
import Position exposing (..)
import Internal.TextBuffer as B exposing (TextBuffer, Patch(..))
import Window as Win exposing (Size)
import Task
import Dict exposing (Dict)
import Vim.AST as V exposing (VisualType(..))
import Syntax exposing (..)
import Elm.Array as Array
import Persistent exposing (getBuffer)
import Json.Encode as Encode
import Fuzzy exposing (FuzzyMatchItem)
import Elm.Array exposing (Array)


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


type alias AutoComplete =
    { source : List String
    , matches : Array FuzzyMatchItem
    , select : Int
    , scrollTop : Int
    }


type Mode
    = Normal
    | Visual VisualMode
    | Insert
    | TempNormal
    | Ex ExMode


type alias ExMode =
    { prefix : ExPrefix
    , exbuf : Buffer
    , visual : Maybe VisualMode
    , autoComplete : Maybe AutoComplete
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
    , showTip : Bool
    }


type alias Model =
    Buffer


type alias BufferHistory =
    { undoes : List Undo
    , pending : Maybe Undo
    , redoes : List Redo
    , savePoint : Int
    , version : Int
    }


emptyBufferHistory : BufferHistory
emptyBufferHistory =
    { undoes = []
    , pending = Nothing
    , redoes = []
    , savePoint = 0
    , version = 0
    }


type RegisterText
    = Text String
    | Lines String


type alias Buffer =
    { lines : TextBuffer
    , syntax : Syntax
    , syntaxDirtyFrom : Maybe Int
    , lintItems : List LocationItem
    , lintErrorsCount : Int
    , cursor : Position
    , cursorColumn : Int
    , path : String
    , name : String
    , mode : Mode
    , history : BufferHistory
    , config : BufferConfig
    , view : View
    , continuation : String
    , registers : Dict String RegisterText
    , dotRegister : String
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
    , service : String
    , syntaxService : String
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
    , showTip = False
    }


encodeBuffer : Buffer -> Encode.Value
encodeBuffer buf =
    let
        ( y, x ) =
            buf.cursor
    in
        Encode.object
            [ ( "path", Encode.string buf.path )
            , ( "cursor"
              , Encode.list <|
                    [ Encode.int y
                    , Encode.int x
                    ]
              )
            , ( "scrollTop"
              , Encode.int buf.view.scrollTop
              )
            ]


type alias BufferConfig =
    { wordChars : String -- a-z and A-Z are word chars by default
    , tabSize : Int
    , expandTab : Bool
    , lint : Bool
    , tokenizeLinesAhead : Int
    }


defaultBufferConfig : BufferConfig
defaultBufferConfig =
    { wordChars = "_"
    , tabSize = 2
    , expandTab = True
    , lint = False
    , tokenizeLinesAhead = 20
    }


emptyBuffer : Buffer
emptyBuffer =
    { lines = B.fromString B.lineBreak
    , syntax = Array.empty
    , syntaxDirtyFrom = Nothing
    , lintItems = []
    , lintErrorsCount = 0
    , cursor = ( 0, 0 )
    , cursorColumn = 0
    , path = ""
    , name = "no name"
    , mode = Normal
    , history = emptyBufferHistory
    , config = defaultBufferConfig
    , view = emptyView
    , continuation = ""
    , registers = Dict.empty
    , dotRegister = ""
    , last =
        { matchChar = Nothing
        , matchString = Nothing
        , inserts = ""
        , visual = ""
        , ex = ""
        , indent = 0
        }
    , vimASTCache = Dict.empty
    , service = ""
    , syntaxService = ""
    }


type alias Flags =
    { lineHeight : Int
    , service : String
    , syntaxService : String
    , buffer : Maybe String
    }


init : Flags -> ( Model, Cmd Msg )
init { lineHeight, service, syntaxService, buffer } =
    let
        view =
            emptyBuffer.view

        buf =
            { emptyBuffer
                | view = { view | lineHeight = lineHeight }
                , service = service
                , syntaxService = syntaxService
            }

        cmds =
            case buffer of
                Just path ->
                    [ getBuffer path ]

                _ ->
                    []
    in
        ( buf
        , Cmd.batch <|
            [ Task.perform Resize Win.size
            ]
                ++ cmds
        )


updateView : (View -> View) -> Buffer -> Buffer
updateView f buf =
    let
        view =
            buf.view
    in
        { buf | view = f buf.view }
