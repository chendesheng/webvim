module Model exposing (..)

import Update.Message exposing (Msg(..), BufferInfo, LintError, bufferInfoDecoder)
import Internal.Position exposing (..)
import Internal.TextBuffer as B exposing (TextBuffer, Patch(..))
import Window as Win exposing (Size)
import Dict exposing (Dict)
import Vim.AST as V exposing (VisualType(..))
import Internal.Syntax exposing (..)
import Elm.Array as Array
import Json.Encode as Encode
import Helper.Fuzzy exposing (FuzzyMatchItem)
import Elm.Array exposing (Array)
import Internal.Jumps exposing (..)
import Dict exposing (Dict)
import Json.Encode as Encode
import Json.Decode as Decode


type alias Undo =
    List Patch


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
    , matchedCursor : Maybe Position
    }


type alias Model =
    Buffer


type alias BufferHistory =
    { undoes : List Undo
    , pending : Undo
    , redoes : List Redo
    , savePoint : Int
    , version : Int
    }


emptyBufferHistory : BufferHistory
emptyBufferHistory =
    { undoes = []
    , pending = []
    , redoes = []
    , savePoint = 0
    , version = 0
    }


type RegisterText
    = Text String
    | Lines String


type alias BufferLint =
    { items : List LintError
    , count : Int
    }


type alias Buffer =
    { lines : TextBuffer
    , syntax : Syntax
    , syntaxDirtyFrom : Int
    , lint : BufferLint
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
        , jumpToTag : Maybe Location
        }
    , vimASTCache : Dict ( String, String ) ( V.AST, String )
    , jumps : Jumps
    , buffers : Dict String BufferInfo
    , locationList : List Location
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
    , matchedCursor = Nothing
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
    , service : String
    , syntaxService : String
    }


defaultBufferConfig : BufferConfig
defaultBufferConfig =
    { wordChars = "_"
    , tabSize = 2
    , expandTab = True
    , lint = False
    , service = ""
    , syntaxService = ""
    }


emptyBuffer : Buffer
emptyBuffer =
    { lines = B.fromString B.lineBreak
    , syntax = Array.empty
    , syntaxDirtyFrom = 0
    , lint = { items = [], count = 0 }
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
        , jumpToTag = Nothing
        }
    , vimASTCache = Dict.empty
    , jumps =
        { backwards = []
        , forwards = []
        }
    , buffers = Dict.empty
    , locationList = []
    }


updateView : (View -> View) -> Buffer -> Buffer
updateView f buf =
    let
        view =
            buf.view
    in
        { buf | view = f buf.view }


registerString : RegisterText -> String
registerString reg =
    case reg of
        Text s ->
            s

        Lines s ->
            s


registerToString : Dict String RegisterText -> String
registerToString registers =
    registers
        |> Dict.toList
        |> List.map
            (\item ->
                let
                    ( k, v ) =
                        item
                in
                    Encode.object
                        [ ( "name", Encode.string k )
                        , case v of
                            Text s ->
                                ( "type", Encode.string "text" )

                            Lines s ->
                                ( "type", Encode.string "lines" )
                        , case v of
                            Text s ->
                                ( "value", Encode.string s )

                            Lines s ->
                                ( "value", Encode.string s )
                        ]
            )
        |> Encode.list
        |> Encode.encode 0


registersDecoder : Decode.Decoder (Dict String RegisterText)
registersDecoder =
    Decode.map3
        (\name tipe value ->
            ( name
            , case tipe of
                "text" ->
                    Text value

                _ ->
                    Lines value
            )
        )
        (Decode.field "name" Decode.string)
        (Decode.field "type" Decode.string)
        (Decode.field "value" Decode.string)
        |> Decode.list
        |> Decode.map Dict.fromList
