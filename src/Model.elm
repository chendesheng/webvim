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
import Helper.Helper exposing (findFirst, charWidthType)


type alias Size =
    { width : Int, height : Int }


type alias CodePoint =
    Int


type alias FontInfo =
    { name : String
    , widths : List ( String, Float )
    , lineHeight : Int
    , size : Int -- pt
    }


charWidth : FontInfo -> Char -> Float
charWidth { widths } ch =
    let
        dict =
            Dict.fromList widths

        codePoint =
            Char.toCode ch

        widthType =
            charWidthType ch
    in
        widths
            |> findFirst
                (\( tipe, width ) ->
                    tipe == widthType
                )
            |> Maybe.map Tuple.second
            |> Maybe.withDefault 10


stringWidth : FontInfo -> Int -> Int -> String -> Int
stringWidth fontInfo b e s =
    s
        |> String.slice b e
        |> String.toList
        |> List.map (charWidth fontInfo)
        |> List.sum
        |> round


cursorCharWidth : FontInfo -> Int -> String -> Int
cursorCharWidth fontInfo x s =
    s
        |> String.dropLeft x
        |> String.uncons
        |> Maybe.map (Tuple.first >> charWidth fontInfo)
        |> Maybe.withDefault (charWidth fontInfo '0')
        |> round


type alias Flags =
    { service : String
    , buffers : Encode.Value
    , activeBuffer : Encode.Value
    , registers : Encode.Value
    , height : Int
    , cwd : String
    , pathSeperator : String
    , fontInfo : FontInfo
    , homedir : String
    , isSafari : Bool
    , exHistory : List String
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
    , ( "cursor", cursorEncoder info.cursor )
    , ( "syntax", Encode.bool info.syntax )
    , ( "history", historyEncoder info.history )
    ]
        |> Encode.object


bufferInfoToString : BufferInfo -> String
bufferInfoToString info =
    info
        |> bufferInfoEncoder
        |> Encode.encode 0


cursorEncoder : Position -> Encode.Value
cursorEncoder ( y, x ) =
    Encode.list Encode.int
        [ y, x ]


cursorDecoder : Decode.Decoder ( Int, Int )
cursorDecoder =
    Decode.list Decode.int
        |> Decode.map
            (\xs ->
                case xs of
                    a :: b :: _ ->
                        ( a, b )

                    _ ->
                        ( 0, 0 )
            )


bufferInfoDecoder : Decode.Decoder BufferInfo
bufferInfoDecoder =
    Decode.map4
        (\path cursor syntax history ->
            { path = path
            , cursor = cursor
            , content = Nothing
            , syntax = syntax
            , history = history
            }
        )
        (Decode.field "path" Decode.string)
        (Decode.field "cursor" cursorDecoder)
        (Decode.field "syntax" Decode.bool)
        (Decode.field "history" historyDecoder)


type alias Key =
    String


type alias BufferInfo =
    { path : String
    , cursor : Position
    , content : Maybe ( B.TextBuffer, Syntax )
    , syntax : Bool
    , history : BufferHistory
    }


emptyBufferInfo : BufferInfo
emptyBufferInfo =
    { path = ""
    , cursor = ( 0, 0 )
    , content = Nothing
    , syntax = True
    , history = emptyBufferHistory
    }


type alias Undo =
    { cursor : Position
    , patches : List Patch
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

    -- if trigger changed then source list need re-populated
    , trigger : String
    , menuLeftOffset : Int
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


type alias View =
    { scrollTop : Int
    , scrollTopPx : Int
    , scrollLeft : Int
    , matchedCursor : Maybe ( Position, Position )
    , lines : List Int
    , size : Size

    -- TODO: move to global
    , statusbarHeight : Int
    , showTip : Bool
    , lineHeight : Int
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

    -- changes in current message
    , diff : List Patch

    -- from server
    , lastModified : String

    -- for persistent
    , changes : List Patch
    , pendingChanges : List Patch
    }


emptyBufferHistory : BufferHistory
emptyBufferHistory =
    { undoes = []
    , pending = { patches = [], cursor = ( 0, 0 ) }
    , redoes = []
    , savePoint = 0
    , version = 0
    , lastModified = ""
    , changes = []
    , pendingChanges = []
    , diff = []
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
    , cursor : ( Int, Int )
    , cursorColumn : Int
    , path : String
    , name : String
    , mode : Mode
    , history : BufferHistory
    , config : BufferConfig
    , view : View
    , continuation : String
    , dirtyIndent : Int
    , motionFailed : Bool
    , global : Global
    }


type alias Global =
    { registers : Dict String RegisterText
    , ime : IME
    , dotRegister : String
    , buffers : Dict String BufferInfo
    , cwd : String
    , exHistory : List String
    , searchHistory : List String
    , service : String
    , pathSeperator : String
    , fontInfo : FontInfo
    , homedir : String
    , isSafari : Bool
    , vimASTCache : Dict ( String, String ) ( V.AST, String )
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
        , jumpToTag : Maybe Location
        }
    , jumps : Jumps
    , lint : BufferLint
    , locationList : List Location
    }


type alias IME =
    { isActive : Bool
    , isComposing : Bool
    , compositionText : String
    , isSafari : Bool
    }


emptyIme : IME
emptyIme =
    { isActive = False
    , isComposing = False
    , compositionText = ""
    , isSafari = False
    }


cacheVimAST : ( String, String ) -> ( V.AST, String ) -> Global -> Global
cacheVimAST k v gb =
    { gb | vimASTCache = Dict.insert k v gb.vimASTCache }


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


updateIme : (IME -> IME) -> Buffer -> Buffer
updateIme fnupdate buf =
    if fnupdate buf.global.ime == buf.global.ime then
        buf
    else
        let
            global =
                buf.global
        in
            { buf | global = { global | ime = fnupdate buf.global.ime } }


emptyView : View
emptyView =
    { scrollTop = 0
    , scrollTopPx = 0
    , scrollLeft = 0
    , showTip = False
    , matchedCursor = Nothing
    , lines = [ 0, 1, 2 ]
    , size = { width = 1, height = 1 }
    , statusbarHeight = 1
    , lineHeight = 21
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
    , indent : IndentConfig
    , syntax : Bool
    }


defaultBufferConfig : BufferConfig
defaultBufferConfig =
    { wordChars = "_"
    , tabSize = 2
    , expandTab = True
    , lint = False
    , indent = AutoIndent
    , syntax = True
    }


emptyBuffer : Buffer
emptyBuffer =
    { lines = B.fromString B.lineBreak
    , syntax = Array.empty
    , syntaxDirtyFrom = 0
    , cursor = ( 0, 0 )
    , cursorColumn = 0
    , path = ""
    , name = "no name"
    , mode = Normal { message = EmptyMessage }
    , history = emptyBufferHistory
    , config = defaultBufferConfig
    , view = emptyView
    , continuation = ""

    -- temp variable
    -- insert mode auto indent, discard when input nothing and switch back to normal mode
    , dirtyIndent = 0
    , motionFailed = False

    -- global state will persist when swithing between buffers
    , global = emptyGlobal
    }


emptyGlobal : Global
emptyGlobal =
    { dotRegister = ""
    , ime = emptyIme
    , buffers = Dict.empty
    , cwd = ""
    , exHistory = []
    , service = ""
    , pathSeperator = "/"
    , fontInfo =
        { widths = []
        , lineHeight = 0
        , size = 0
        , name = ""
        }
    , homedir = ""
    , isSafari = False
    , registers = Dict.empty
    , searchHistory = []
    , last =
        { matchChar = Nothing
        , matchString = Nothing
        , inserts = ""
        , visual = ""
        , ex = ""
        , jumpToTag = Nothing
        }
    , vimASTCache = Dict.empty

    -- TODO: add a location pool
    -- locations : Dict BufferId (Dict Int Position)
    , jumps =
        { backwards = []
        , forwards = []
        }
    , lint = { items = [], count = 0 }
    , locationList = []
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


historyEncoder : BufferHistory -> Encode.Value
historyEncoder { version, savePoint, undoes, redoes, changes, lastModified } =
    Encode.object
        [ ( "version", Encode.int version )
        , ( "savePoint", Encode.int savePoint )
        , ( "undoes", Encode.list undoEncoder undoes )
        , ( "redoes", Encode.list undoEncoder redoes )
        , ( "changes", Encode.list patchEncoder changes )
        , ( "lastModified", Encode.string lastModified )
        ]


historyDecoder : Decode.Decoder BufferHistory
historyDecoder =
    Decode.map6
        (\version savePoint undoes redoes changes lastModified ->
            { undoes = undoes
            , pending = emptyUndo
            , redoes = redoes
            , savePoint = savePoint
            , version = version
            , lastModified = lastModified
            , changes = changes
            , pendingChanges = []
            , diff = []
            }
        )
        (Decode.field "version" Decode.int)
        (Decode.field "savePoint" Decode.int)
        (Decode.field "undoes" <| Decode.list undoDecoder)
        (Decode.field "redoes" <| Decode.list undoDecoder)
        (Decode.field "changes" <| Decode.list patchDecoder)
        (Decode.field "lastModified" Decode.string)


patchEncoder : Patch -> Encode.Value
patchEncoder p =
    case p of
        Deletion b e ->
            Encode.object
                [ ( "type", Encode.string "-" )
                , ( "b", cursorEncoder b )
                , ( "e", cursorEncoder e )
                ]

        Insertion pos s ->
            Encode.object
                [ ( "type", Encode.string "+" )
                , ( "pos", cursorEncoder pos )
                , ( "s", Encode.string <| B.toString s )
                ]


patchDecoder : Decode.Decoder Patch
patchDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typ ->
                if typ == "+" then
                    Decode.map2 Insertion
                        (Decode.field "pos" cursorDecoder)
                        (Decode.field "s" Decode.string
                            |> Decode.map B.fromString
                        )
                else if typ == "-" then
                    Decode.map2 Deletion
                        (Decode.field "b" cursorDecoder)
                        (Decode.field "e" cursorDecoder)
                else
                    Decode.fail <| "unknown type: " ++ typ
            )


undoDecoder : Decode.Decoder Undo
undoDecoder =
    Decode.map2 Undo
        (Decode.field "cursor" <| cursorDecoder)
        (Decode.field "patches" <| Decode.list patchDecoder)


undoEncoder : Undo -> Encode.Value
undoEncoder { cursor, patches } =
    Encode.object
        [ ( "cursor", cursorEncoder cursor )
        , ( "patches", Encode.list patchEncoder patches )
        ]
