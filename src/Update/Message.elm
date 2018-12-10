module Update.Message exposing (..)

import Result
import Http
import Internal.Syntax exposing (Token, Syntax)
import Vim.AST exposing (AST)
import Internal.Jumps exposing (Location)
import Internal.TextBuffer as B exposing (Patch)
import Model exposing (Key, LintError, Buffer, Flags, Size, IME)
import Internal.Window exposing (Path)


type alias BufferIdentifier =
    -- (bufId, version)
    ( Int, Int )


type TokenizeResponse
    = TokenizeSuccess Int Syntax
    | TokenizeLineSuccess Int (List Token)
    | TokenizeCacheMiss -- happens when server restart
    | TokenizeError String


type alias TokenizeRequest =
    { bufId : Int
    , path : String
    , version : Int
    , line : Int
    , lines : String
    }


type IMEMsg
    = CompositionWait String
    | CompositionTry String
    | CompositionStart String
    | CompositionCommit String
    | CompositionEnd
    | IMEFocus


type Msg
    = PressKeys Key -- buffer id, key
    | IMEMessage IMEMsg
    | Resize Size
    | Read (Result Http.Error Buffer)
    | Write (Result String ( String, List Patch ))
    | MakeDir (Result String ())
    | ReadClipboard
        (Result Http.Error
            { replaying : Bool
            , key : Key
            , ast : AST
            , s : String
            }
        )
    | WriteClipboard (Result String ())
    | SendLint
    | SendTokenize
    | Lint BufferIdentifier (Result String (List LintError))
    | Tokenized BufferIdentifier (Result String TokenizeResponse)
    | ListFiles (Result String (List String))
    | ListDirectries (Result String (List String))
    | ListAllFiles (Result String (List String)) -- recursive
    | ListBuffers (List String)
    | ReadTags (Result String Location)
    | SearchResult (Result String String)
    | SetCwd (Result String String)
    | Boot (Result String Flags)
    | MouseWheel Path Int
    | NoneMessage
