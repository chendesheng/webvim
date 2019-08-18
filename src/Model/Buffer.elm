module Model.Buffer exposing
    ( AutoComplete
    , Buffer
    , ExMode
    , ExPrefix(..)
    , Mode(..)
    , StatusMessage(..)
    , VisualMode
    , bufferDecoder
    , bufferEncoder
    , bufferToString
    , buffersToString
    , emptyBuffer
    , emptyExBuffer
    , getModeName
    , isExMode
    , isExculdLineBreak
    , isLintEnabled
    , setExbuf
    , stringToPrefix
    )

import Array as Array
import Dict
import Fs exposing (FileSystem)
import Helper.Fuzzy exposing (FuzzyMatchItem)
import Helper.Helper exposing (extname, filename, relativePath)
import Internal.Position exposing (..)
import Internal.Syntax exposing (..)
import Internal.TextBuffer as B exposing (Patch(..), TextBuffer)
import Json.Decode as Decode
import Json.Encode as Encode
import Menu as Mu
import Model.BufferConfig exposing (..)
import Model.BufferHistory exposing (..)
import Model.View exposing (..)
import TreeSitter as TS
import Vim.AST as V exposing (VisualType(..))


isLintEnabled : FileSystem -> String -> Bool -> Bool
isLintEnabled fs name lint =
    if lint && extname name == ".elm" then
        name
            |> relativePath (Fs.pathSeperator fs) (Fs.homeDir fs)
            |> String.startsWith (".elm" ++ Fs.pathSeperator fs)
            |> not

    else
        lint


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


type StatusMessage
    = InfoMessage String
    | ErrorMessage String
    | EmptyMessage


type ExPrefix
    = ExSearch
        { forward : Bool
        , match : Maybe ( Position, Position ) -- increment cursor position
        , highlights : List ( Position, Position )

        -- , scrollTop
        }
    | ExCommand
    | ExEval


type alias ExMode =
    { prefix : ExPrefix
    , exbuf : Buffer
    , visual : Maybe VisualMode
    , message : StatusMessage
    }


setExbuf : Buffer -> ExMode -> Buffer -> Buffer
setExbuf buf ex exbuf =
    { buf | mode = Ex { ex | exbuf = exbuf } }


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


type alias Buffer =
    { id : String
    , lines : TextBuffer
    , syntax : Syntax
    , syntaxDirtyFrom : Int
    , path : String -- absolute path
    , name : String
    , mode : Mode
    , history : BufferHistory
    , config : BufferConfig
    , view : View -- temp view
    , continuation : String
    , dirtyIndent : Int
    , motionFailed : Bool
    , treeSitter :
        { parser : TS.Parser
        , tree : TS.Tree
        }
    }


emptyBuffer : Buffer
emptyBuffer =
    { id = ""
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
    , treeSitter =
        { parser = TS.dummyParser
        , tree = TS.parse TS.dummyParser (always "")
        }
    }


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


isExMode mode =
    case mode of
        Ex _ ->
            True

        _ ->
            False


bufferDecoder : FileSystem -> Decode.Decoder Buffer
bufferDecoder fs =
    Decode.map4
        (\id path syntax history ->
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
                , config =
                    { config
                        | lint = isLintEnabled fs (name ++ ext) config.lint
                        , syntax = syntax
                    }
            }
        )
        (Decode.field "id" Decode.string)
        (Decode.field "path" Decode.string)
        (Decode.field "syntax" Decode.bool)
        (Decode.field "history" historyDecoder)


buffersToString : List Buffer -> String
buffersToString buffers =
    buffers
        |> Encode.list bufferEncoder
        |> Encode.encode 0


bufferEncoder : Buffer -> Encode.Value
bufferEncoder buf =
    Encode.object
        [ ( "id", Encode.string buf.id )
        , ( "path", Encode.string buf.path )
        , ( "syntax", Encode.bool buf.config.syntax )
        , ( "history", historyEncoder buf.history )
        , ( "view", viewEncoder buf.view )
        ]


bufferToString buf =
    buf
        |> bufferEncoder
        |> Encode.encode 0


isExculdLineBreak : Mode -> Bool
isExculdLineBreak mode =
    case mode of
        Visual _ ->
            False

        Insert _ ->
            False

        _ ->
            True


getModeName : Mode -> V.ModeName
getModeName mode =
    case mode of
        Normal _ ->
            V.ModeNameNormal

        Insert _ ->
            V.ModeNameInsert

        TempNormal ->
            V.ModeNameTempNormal

        Visual { tipe } ->
            V.ModeNameVisual tipe

        Ex { prefix } ->
            V.ModeNameEx <| prefixToString prefix


stringToPrefix : String -> ExPrefix
stringToPrefix prefix =
    case prefix of
        "/" ->
            ExSearch { forward = True, match = Nothing, highlights = [] }

        "?" ->
            ExSearch { forward = False, match = Nothing, highlights = [] }

        "=" ->
            ExEval

        _ ->
            ExCommand


prefixToString : ExPrefix -> String
prefixToString prefix =
    case prefix of
        ExSearch { forward } ->
            if forward then
                "/"

            else
                "?"

        ExEval ->
            "="

        ExCommand ->
            ":"
