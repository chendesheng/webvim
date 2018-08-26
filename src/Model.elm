module Model exposing (..)

-- types referenced from Model should be here (expect internal types like Patch)
-- types only part of a message (like tokenize result) should be in
--   Update.Message module
-- Model should not import Update.Message

import Internal.Position exposing (..)
import Internal.TextBuffer as B exposing (TextBuffer, Patch(..))
import Dict exposing (Dict)
import Vim.AST as V exposing (VisualType(..))
import Internal.Syntax exposing (..)
import Array as Array exposing (Array)
import Json.Encode as Encode
import Helper.Fuzzy exposing (FuzzyMatchItem)
import Internal.Jumps exposing (..)
import Dict exposing (Dict)
import Json.Encode as Encode
import Json.Decode as Decode
import Regex as Re


type alias Size =
    { width : Int, height : Int }


type alias Flags =
    { lineHeight : Int
    , service : String
    , buffers : Encode.Value
    , activeBuffer : Encode.Value
    , registers : Encode.Value
    , height : Int
    , cwd : String
    , pathSeperator : String
    }


type alias TextWithStyle =
    { bold : Bool
    , color : Maybe String
    , underline : Bool
    , string : String
    }


type RichText
    = PlainText String
    | RichText (List TextWithStyle)


type alias LintError =
    { tipe : String
    , tag : Maybe String
    , file : String
    , overview : String
    , details : RichText
    , region : ( Position, Position )
    , subRegion : Maybe ( Position, Position )
    }


buffersInfoToString : List BufferInfo -> String
buffersInfoToString buffers =
    buffers
        |> Encode.list bufferInfoEncoder
        |> Encode.encode 0


bufferInfoEncoder : BufferInfo -> Encode.Value
bufferInfoEncoder info =
    [ ( "path", Encode.string info.path )
    , ( "version", Encode.int info.version )
    , ( "cursor"
      , Encode.list Encode.int
            [ info.cursor |> Tuple.first
            , info.cursor |> Tuple.second
            ]
      )
    , ( "syntax", Encode.bool info.syntax )
    ]
        |> Encode.object


bufferInfoToString : BufferInfo -> String
bufferInfoToString info =
    info
        |> bufferInfoEncoder
        |> Encode.encode 0


bufferInfoDecoder : Decode.Decoder BufferInfo
bufferInfoDecoder =
    Decode.map4
        (\path version cursor syntax ->
            { path = path
            , version = version
            , cursor = cursor
            , content = Nothing
            , syntax = syntax
            }
        )
        (Decode.field "path" Decode.string)
        (Decode.field "version" Decode.int)
        (Decode.field "cursor" (Decode.list Decode.int)
            |> Decode.map
                (\xs ->
                    case xs of
                        a :: b :: _ ->
                            ( a, b )

                        _ ->
                            ( 0, 0 )
                )
        )
        (Decode.field "syntax" Decode.bool)


type alias Key =
    String


type alias BufferInfo =
    { path : String
    , cursor : Position
    , content : Maybe ( B.TextBuffer, Syntax )
    , version : Int
    , syntax : Bool
    }


emptyBufferInfo : BufferInfo
emptyBufferInfo =
    { path = ""
    , cursor = ( 0, 0 )
    , content = Nothing
    , version = 0
    , syntax = True
    }


type alias Undo =
    { patches : List Patch
    , cursor : Position
    }


emptyUndo : Undo
emptyUndo =
    { patches = [], cursor = ( 0, 0 ) }


type alias Redo =
    Undo


type ExPrefix
    = ExSearch
        { forward : Bool
        , match : Maybe ( Position, Position ) -- increment cursor position
        , highlights : List ( Position, Position )
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
    , pos : Position
    , wordChars : String
    }


type Mode
    = Normal { message : StatusMessage }
    | Visual VisualMode
    | Insert
        { autoComplete : Maybe AutoComplete
        , startCursor : Position -- cursor position when enter insert mode
        , visual : Maybe VisualMode
        }
    | TempNormal
    | Ex ExMode


type alias ExMode =
    { prefix : ExPrefix
    , exbuf : Buffer
    , visual : Maybe VisualMode
    , message : StatusMessage
    }


type alias ViewLine =
    { lineNumber : Int
    , text : String
    , tokens : List Token
    }


type alias View =
    { scrollTop : Int

    -- TODO: remove scrollTop
    , scrollTopPx : Int
    , scrollLeft : Int
    , startPosition : Position
    , dataStartPosition : Position
    , size : Size
    , statusbarHeight : Int

    -- pixel height of a line
    , lineHeight : Int
    , showTip : Bool
    , matchedCursor : Maybe ( Position, Position )
    , lines : List (Maybe ViewLine)
    }


type Model
    = Booting
    | Ready Buffer
    | Crashed String


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
    , pending = { patches = [], cursor = ( 0, 0 ) }
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

        -- if motion failed don't change to insert mode
        , motionFailed : Bool
        }
    , vimASTCache : Dict ( String, String ) ( V.AST, String )
    , jumps : Jumps
    , buffers : Dict String BufferInfo
    , locationList : List Location
    , cwd : String
    }


cacheVimAST : ( String, String ) -> ( V.AST, String ) -> Buffer -> Buffer
cacheVimAST k v buf =
    { buf | vimASTCache = Dict.insert k v buf.vimASTCache }


emptyExBuffer : Buffer
emptyExBuffer =
    { emptyBuffer
        | mode =
            Insert
                { autoComplete = Nothing
                , startCursor = ( 0, 0 )
                , visual = Nothing
                }
        , lines = B.empty
    }


emptyView : View
emptyView =
    { scrollTop = 0
    , scrollTopPx = 0
    , scrollLeft = 0
    , startPosition = ( 0, 0 )
    , size = { width = 1, height = 1 }
    , dataStartPosition = ( 0, 0 )
    , statusbarHeight = 1
    , lineHeight = 21
    , showTip = False
    , matchedCursor = Nothing
    , lines =
        [ Just { lineNumber = 0, text = "\n", tokens = [] }
        , Just { lineNumber = 1, text = "", tokens = [] }
        , Nothing
        ]
    }


type IndentConfig
    = AutoIndent -- same indent as last line
    | IndentRules
        { increase : Re.Regex
        , increaseNext : Re.Regex
        , decrease : Re.Regex
        }


type alias BufferConfig =
    { wordChars : String -- a-z and A-Z are word chars by default
    , tabSize : Int
    , expandTab : Bool
    , lint : Bool
    , service : String
    , pathSeperator : String
    , indent : IndentConfig
    , syntax : Bool
    }


defaultBufferConfig : BufferConfig
defaultBufferConfig =
    { wordChars = "_"
    , tabSize = 2
    , expandTab = True
    , lint = False
    , service = ""
    , pathSeperator = "/"
    , indent = AutoIndent
    , syntax = True
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
    , mode = Normal { message = EmptyMessage }
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
        , motionFailed = False
        }
    , vimASTCache = Dict.empty
    , jumps =
        { backwards = []
        , forwards = []
        }
    , buffers = Dict.empty
    , locationList = []
    , cwd = ""
    }


type StatusMessage
    = InfoMessage String
    | ErrorMessage String
    | EmptyMessage


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
        |> Encode.list
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
