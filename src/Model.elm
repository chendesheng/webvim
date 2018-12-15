module Model exposing
    ( AutoComplete
    , Buffer
    , BufferConfig
    , BufferHistory
    , BufferLint
    , CodePoint
    , Editor
    , ExMode
    , ExPrefix(..)
    , Flags
    , FontInfo
    , Global
    , IME
    , IndentConfig(..)
    , Key
    , LintError
    , Mode(..)
    , Model(..)
    , Redo
    , RegisterText(..)
    , RichText(..)
    , Size
    , StatusMessage(..)
    , TextWithStyle
    , Undo
    , View
    , VisualMode
    , bufferDecoder
    , bufferEncoder
    , bufferToString
    , buffersToString
    , cIndentRules
    , cacheVimAST
    , charWidth
    , configs
    , createBuffer
    , cssFileDefaultConfig
    , cursorCharWidth
    , cursorDecoder
    , cursorEncoder
    , defaultBufferConfig
    , emptyBuffer
    , emptyBufferHistory
    , emptyExBuffer
    , emptyGlobal
    , emptyIme
    , emptyUndo
    , emptyView
    , getActiveBuffer
    , historyDecoder
    , historyEncoder
    , increaseMaxId
    , isLintEnabled
    , isTempBuffer
    , patchDecoder
    , patchEncoder
    , persistentAll
    , registerString
    , registerToString
    , registersDecoder
    , setBuffer
    , stringWidth
    , undoDecoder
    , undoEncoder
    , updateBuffer
    , updateGlobal
    , updateIme
    , updateWindow
    , viewDecoder
    , viewEncoder
    , windowDecoder
    , windowEncoder
    )

-- types referenced from Model should be here (expect internal types like Patch)
-- types only part of a message (like tokenize result) should be in
--   Update.Message module
-- Model should not import Update.Message

import Array as Array exposing (Array)
import Dict exposing (Dict)
import Helper.Fuzzy exposing (FuzzyMatchItem)
import Helper.Helper exposing (charWidthType, extname, filename, findFirst, regex, relativePath)
import Internal.Jumps exposing (..)
import Internal.Position exposing (..)
import Internal.Syntax exposing (..)
import Internal.TextBuffer as B exposing (Patch(..), RegionChange, TextBuffer)
import Internal.Window as Win exposing (Window)
import Json.Decode as Decode
import Json.Encode as Encode
import Regex as Re
import Vim.AST as V exposing (VisualType(..))


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
    , window : Encode.Value
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


buffersToString : List Buffer -> String
buffersToString buffers =
    buffers
        |> Encode.list bufferEncoder
        |> Encode.encode 0


bufferEncoder : Buffer -> Encode.Value
bufferEncoder buf =
    Encode.object
        [ ( "id", Encode.int buf.id )
        , ( "path", Encode.string buf.path )
        , ( "syntax", Encode.bool buf.config.syntax )
        , ( "history", historyEncoder buf.history )
        , ( "view", viewEncoder buf.view )
        ]


bufferToString : Buffer -> String
bufferToString buf =
    buf
        |> bufferEncoder
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


cIndentRules :
    { decrease : Re.Regex
    , increase : Re.Regex
    , increaseNext : Re.Regex
    , trigger : String
    }
cIndentRules =
    { increase = regex "^.*\\{[^}\\\"']*$"
    , decrease = regex "^(.*\\*/)?\\s*\\}[;\\s]*$"
    , increaseNext =
        regex "^(?!.*;\\s*//).*[^\\s;{}]\\s*$"
    , trigger = "}"
    }


cssFileDefaultConfig : BufferConfig
cssFileDefaultConfig =
    { defaultBufferConfig
        | tabSize = 2
        , indent = IndentRules cIndentRules
        , wordChars = "_-.#"
    }


configs : Dict String BufferConfig
configs =
    Dict.fromList
        [ ( ".elm"
          , { defaultBufferConfig
                | tabSize = 4
                , lint = True
                , indent =
                    IndentRules
                        { increase =
                            regex
                                ("(^[(]?let$)|(^[(]?if)"
                                    ++ "|(^then$)|(^else(\\s|$))|(=$)"
                                    ++ "|(^in$)|(^[(]?case)|(^of$)|(->$)"
                                )
                        , decrease = regex "^(then|else( if)?|of|in)"
                        , increaseNext = regex "![\\s\\S]"
                        , trigger = ""
                        }
            }
          )
        , ( ".js"
          , { defaultBufferConfig
                | tabSize = 2
                , lint = True
                , indent = IndentRules cIndentRules
            }
          )
        , ( ".jsx"
          , { defaultBufferConfig
                | tabSize = 2
                , lint = True
                , indent = IndentRules cIndentRules
            }
          )
        , ( ".purs"
          , { defaultBufferConfig
                | tabSize = 2
                , indent =
                    IndentRules
                        { increase =
                            regex
                                ("(^[(]?let$)|(^[(]?if)"
                                    ++ "|(^then$)|(^else(\\s|$))|(=$)"
                                    ++ "|(^in$)|(^[(]?case)|(^of$)|(->$)"
                                    ++ "|(^when)|(\\sdo$)"
                                )
                        , decrease = regex "^(then|else( if)?|of|in)"
                        , increaseNext = regex "![\\s\\S]"
                        , trigger = ""
                        }
            }
          )
        , ( ".less", cssFileDefaultConfig )
        , ( ".css", cssFileDefaultConfig )
        ]


isLintEnabled : String -> String -> String -> Bool -> Bool
isLintEnabled pathSeperator homedir name lint =
    if lint && extname name == ".elm" then
        name
            |> relativePath pathSeperator homedir
            |> String.startsWith (".elm" ++ pathSeperator)
            |> not

    else
        lint


increaseMaxId : Global -> Global
increaseMaxId global =
    { global | maxId = global.maxId + 1 }


createBuffer : String -> Size -> Global -> ( Global, Buffer )
createBuffer path size global =
    let
        ( name, ext ) =
            filename path

        config =
            configs
                |> Dict.get ext
                |> Maybe.withDefault defaultBufferConfig

        global1 =
            increaseMaxId global
    in
    ( global1
    , { emptyBuffer
        | id = global1.maxId
        , view =
            { emptyView
                | bufId = global1.maxId
                , lines = List.range 0 (size.height + 1)
                , size = size
            }
        , config =
            { config
                | lint =
                    isLintEnabled global.pathSeperator
                        global.homedir
                        (name ++ ext)
                        config.lint
                , syntax = not <| isTempBuffer path
            }
        , path = path
        , name = name ++ ext
      }
    )


viewDecoder : Decode.Decoder View
viewDecoder =
    Decode.map4
        (\bufId cursor scrollTop alternativeBuf ->
            { emptyView
                | bufId = bufId
                , cursor = cursor
                , cursorColumn = Tuple.second cursor
                , scrollTop = scrollTop
                , alternativeBuf = alternativeBuf
            }
        )
        (Decode.field "bufId" Decode.int)
        (Decode.field "cursor" cursorDecoder)
        (Decode.field "scrollTop" Decode.int)
        (Decode.field "alternativeBuf" Decode.string |> Decode.maybe)


viewEncoder : View -> Encode.Value
viewEncoder view =
    Encode.object
        ([ ( "bufId", Encode.int view.bufId )
         , ( "cursor", cursorEncoder view.cursor )
         , ( "scrollTop", Encode.int view.scrollTop )
         ]
            ++ (case view.alternativeBuf of
                    Just s ->
                        [ ( "alternativeBuf", Encode.string s ) ]

                    _ ->
                        []
               )
        )


windowEncoder : Window View -> Encode.Value
windowEncoder =
    Win.windowEncoder viewEncoder


windowDecoder : Decode.Decoder (Window View)
windowDecoder =
    Win.windowDecoder viewDecoder


bufferDecoder : String -> String -> Decode.Decoder Buffer
bufferDecoder pathSeperator homedir =
    Decode.map5
        (\id path syntax history view ->
            let
                ( name, ext ) =
                    filename path

                config =
                    configs
                        |> Dict.get ext
                        |> Maybe.withDefault defaultBufferConfig
            in
            { emptyBuffer
                | id = id
                , history = history
                , path = path
                , name = name ++ ext
                , view = view
                , config =
                    { config
                        | lint =
                            isLintEnabled pathSeperator
                                homedir
                                (name ++ ext)
                                config.lint
                        , syntax = syntax
                    }
            }
        )
        (Decode.field "id" Decode.int)
        (Decode.field "path" Decode.string)
        (Decode.field "syntax" Decode.bool)
        (Decode.field "history" historyDecoder)
        (Decode.field "view" viewDecoder)


type alias Key =
    String


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
    { bufId : Int
    , cursor : ( Int, Int )
    , cursorColumn : Int
    , scrollTop : Int
    , scrollTopPx : Int
    , scrollLeft : Int
    , matchedCursor : Maybe ( Position, Position )
    , lines : List Int
    , size : Size

    -- TODO: save buffer id when buffer switch
    --       update '#' register when view switch
    , alternativeBuf : Maybe String
    }


type Model
    = Booting
    | Ready Global
    | Crashed String


type alias BufferHistory =
    { undoes : List Undo
    , pending : Undo
    , redoes : List Redo
    , savePoint : Int
    , version : Int

    -- changes in current message
    , diff : List RegionChange

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


type alias Editor =
    { buf : Buffer
    , global : Global
    }


type alias Buffer =
    { id : Int
    , lines : TextBuffer
    , syntax : Syntax
    , syntaxDirtyFrom : Int
    , path : String
    , name : String
    , mode : Mode
    , history : BufferHistory
    , config : BufferConfig
    , view : View
    , continuation : String
    , dirtyIndent : Int
    , motionFailed : Bool
    }


type alias Global =
    { maxId : Int
    , registers : Dict String RegisterText
    , ime : IME
    , dotRegister : String

    {-
       1. change active view and buffer
       2. keep view change active buffer
    -}
    , window : Window View
    , buffers : Dict Int Buffer
    , cwd : String
    , exHistory : List String
    , searchHistory : List String
    , service : String
    , pathSeperator : String
    , fontInfo : FontInfo
    , homedir : String
    , isSafari : Bool
    , vimASTCache : Dict ( String, String ) ( V.AST, String )
    , size : Size
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
    , statusbarHeight : Int
    , showTip : Bool
    , lineHeight : Int

    -- move window and buffers to here once you need to persistent them to the session storage
    -- this is for performance so you don't need save it on every type
    , persistent :
        Maybe
            { window : Window View
            , buffers : List Buffer
            }
    }


isTempBuffer : String -> Bool
isTempBuffer path =
    String.isEmpty path || path == "[Search]"


getActiveBuffer : Global -> Maybe Buffer
getActiveBuffer global =
    Win.getActiveView global.window
        |> Maybe.andThen
            (\view ->
                Dict.get
                    view.bufId
                    global.buffers
            )


setBuffer : Buffer -> Global -> Global
setBuffer buf global =
    { global
        | buffers = Dict.insert buf.id buf global.buffers
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


updateIme : (IME -> IME) -> Global -> Global
updateIme fnupdate global =
    let
        ime =
            fnupdate global.ime
    in
    if ime == global.ime then
        global

    else
        { global | ime = ime }


updateBuffer : (Buffer -> Buffer) -> Editor -> Editor
updateBuffer fn ed =
    { ed | buf = fn ed.buf }


updateGlobal : (Global -> Global) -> Editor -> Editor
updateGlobal fn ed =
    { ed | global = fn ed.global }


updateWindow : (Window View -> Window View) -> Global -> Global
updateWindow fn global =
    { global | window = fn global.window }


emptyView : View
emptyView =
    { bufId = 0
    , cursor = ( 0, 0 )
    , cursorColumn = 0
    , scrollTop = 0
    , scrollTopPx = 0
    , scrollLeft = 0
    , matchedCursor = Nothing
    , lines = [ 0, 1, 2 ]
    , size = { width = 1, height = 1 }
    , alternativeBuf = Nothing
    }


type IndentConfig
    = AutoIndent -- same indent as last line
    | IndentRules
        { increase : Re.Regex
        , increaseNext : Re.Regex
        , decrease : Re.Regex
        , trigger : String
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
    , syntax = False
    }


emptyBuffer : Buffer
emptyBuffer =
    { id = 0
    , lines = B.fromString B.lineBreak
    , syntax = Array.empty
    , syntaxDirtyFrom = 0
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
    }


emptyGlobal : Global
emptyGlobal =
    { maxId = 0
    , size = { width = 0, height = 0 }
    , window = Win.empty
    , dotRegister = ""
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
    , showTip = False
    , statusbarHeight = 1
    , lineHeight = 21
    , persistent = Nothing
    }


persistentAll : Global -> Global
persistentAll global =
    { global
        | persistent =
            Just
                { window = global.window
                , buffers = Dict.values global.buffers
                }
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
            { emptyBufferHistory
                | undoes = undoes
                , pending = emptyUndo
                , redoes = redoes
                , savePoint = savePoint
                , version = version
                , lastModified = lastModified
                , changes = changes
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
