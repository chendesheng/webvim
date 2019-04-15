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
    , Global
    , IndentConfig(..)
    , Key
    , LintError
    , LoadBuffer(..)
    , Mode(..)
    , Model(..)
    , Redo
    , RegisterText(..)
    , ServerArgs
    , Size
    , StatusMessage(..)
    , TextFragment
    , TextSpan(..)
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
    , configs
    , createBuffer
    , cssFileDefaultConfig
    , cursorDecoder
    , cursorEncoder
    , defaultBufferConfig
    , emptyBuffer
    , emptyBufferHistory
    , emptyExBuffer
    , emptyGlobal
    , emptyUndo
    , emptyView
    , getActiveBuffer
    , getBuffer
    , getBuffers
    , getLoadedBuffer
    , getLoadedBuffers
    , getNotLoadBuffer
    , historyDecoder
    , historyEncoder
    , increaseMaxId
    , isLintEnabled
    , isTempBuffer
    , listBuffers
    , patchDecoder
    , patchEncoder
    , persistentAll
    , registerString
    , registerToString
    , registersDecoder
    , replaceActiveView
    , resizeView
    , setBuffer
    , setExbuf
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
import Boot
import Debouncers exposing (Debouncers, emptyDebouncers)
import Dict exposing (Dict)
import Font exposing (FontInfo)
import Helper.Debounce as Deb
import Helper.Fuzzy exposing (FuzzyMatchItem)
import Helper.Helper
    exposing
        ( extname
        , filename
        , findFirst
        , rangeCount
        , regex
        , relativePath
        )
import Ime exposing (IME, emptyIme)
import Internal.Jumps exposing (..)
import Internal.Position exposing (..)
import Internal.Syntax exposing (..)
import Internal.TextBuffer as B exposing (Patch(..), RegionChange, TextBuffer)
import Internal.Window as Win exposing (Window)
import Json.Decode as Decode
import Json.Encode as Encode
import Menu as Mu
import Regex as Re
import Vim.AST as V exposing (VisualType(..))


type alias Size =
    { width : Int, height : Int }


type alias CodePoint =
    Int


type alias Flags =
    { service : String
    , buffers : Encode.Value
    , window : Encode.Value
    , registers : Encode.Value
    , cwd : String
    , exHistory : List String
    }


type alias ServerArgs =
    { pathSeperator : String
    , homedir : String
    }


type alias TextWithStyle =
    { bold : Bool
    , color : Maybe String
    , underline : Bool
    , string : String
    }


type TextSpan
    = PlainText String
    | RichText TextWithStyle


type alias TextFragment =
    List TextSpan


type alias LintError =
    { tipe : String
    , tag : Maybe String
    , file : String
    , overview : String
    , details : TextFragment
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

        viewLines =
            rangeCount 0 <| size.height + 2
    in
    ( global1
    , { emptyBuffer
        | id = global1.maxId
        , view =
            { emptyView
                | bufId = global1.maxId
                , lines = viewLines
                , gutterLines = viewLines
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

        -- , scrollTop
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
    , menu : Mu.Model FuzzyMatchItem
    , word : String
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


setExbuf : Buffer -> ExMode -> Buffer -> Buffer
setExbuf buf ex exbuf =
    { buf | mode = Ex { ex | exbuf = exbuf } }


type alias View =
    { bufId : Int
    , cursor : ( Int, Int )
    , cursorColumn : Int
    , scrollTop : Int
    , scrollTopPx : Int
    , scrollLeftPx : Int
    , matchedCursor : Maybe ( Position, Position )
    , lines : List Int
    , gutterLines : List Int
    , size : Size

    -- TODO: save buffer id when buffer switch
    --       update '#' register when view switch
    , alternativeBuf : Maybe String
    }


resizeView : Size -> View -> View
resizeView size view =
    if size == view.size then
        view

    else if size.height == view.size.height then
        { view | size = size }

    else
        let
            viewLines =
                rangeCount view.scrollTop <| size.height + 2
        in
        { view
            | size = size
            , lines = viewLines
            , gutterLines = viewLines
        }


replaceActiveView : View -> Win.Window View -> Win.Window View
replaceActiveView view =
    Win.updateActiveView
        (\{ size } -> resizeView size view)


type Model
    = Booting Flags Boot.Model
    | Ready Global


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
    , path : String -- absolute path
    , name : String
    , mode : Mode
    , history : BufferHistory
    , config : BufferConfig
    , view : View
    , continuation : String
    , dirtyIndent : Int
    , motionFailed : Bool
    }


type LoadBuffer
    = NotLoad Buffer
    | Loaded Buffer


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
    , buffers : Dict Int LoadBuffer
    , cwd : String
    , exHistory : List String
    , searchHistory : List String
    , service : String
    , pathSeperator : String
    , fontInfo : FontInfo
    , homedir : String
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
    , theme : String

    -- move window and buffers to here once you need to persistent them to the session storage
    -- this is for performance so you don't need save it on every type
    , persistent :
        Maybe
            { window : Window View
            , buffers : List Buffer
            }
    , debouncers : Debouncers
    }


isTempBuffer : String -> Bool
isTempBuffer path =
    String.isEmpty path || path == "[Search]"


getLoadedBuffer : LoadBuffer -> Maybe Buffer
getLoadedBuffer buf =
    case buf of
        NotLoad _ ->
            Nothing

        Loaded b ->
            Just b


getNotLoadBuffer : LoadBuffer -> Maybe Buffer
getNotLoadBuffer buf =
    case buf of
        NotLoad b ->
            Just b

        Loaded _ ->
            Nothing


getBuffer : Int -> Dict Int LoadBuffer -> Maybe Buffer
getBuffer id =
    Dict.get id
        >> Maybe.andThen getLoadedBuffer


getLoadedBuffers : Dict Int LoadBuffer -> List Buffer
getLoadedBuffers =
    Dict.values >> List.filterMap getLoadedBuffer


getBuffers : Dict Int LoadBuffer -> List Buffer
getBuffers =
    let
        buffer buf =
            case buf of
                NotLoad b ->
                    b

                Loaded b ->
                    b
    in
    Dict.values >> List.map buffer


getActiveBuffer :
    { a | window : Window View, buffers : Dict Int LoadBuffer }
    -> Maybe Buffer
getActiveBuffer { window, buffers } =
    Win.getActiveView window
        |> Maybe.andThen
            (\view ->
                buffers
                    |> Dict.get view.bufId
                    |> Maybe.andThen getLoadedBuffer
            )


listBuffers : Dict Int LoadBuffer -> List ( Buffer, Bool )
listBuffers =
    Dict.values
        >> List.map
            (\buf ->
                case buf of
                    NotLoad b ->
                        ( b, False )

                    Loaded b ->
                        ( b, True )
            )


setBuffer : Buffer -> Global -> Global
setBuffer buf global =
    { global
        | buffers =
            Dict.insert buf.id (Loaded buf) global.buffers
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
    , scrollLeftPx = 0
    , matchedCursor = Nothing
    , lines = rangeCount 0 3
    , gutterLines = rangeCount 0 3
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
    , theme = ""
    , debouncers = emptyDebouncers
    }


persistentAll : Global -> Global
persistentAll global =
    { global
        | persistent =
            Just
                { window = global.window
                , buffers = getBuffers global.buffers
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
