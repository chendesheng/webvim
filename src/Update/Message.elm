module Update.Message exposing (..)

import Window exposing (Size)
import Result
import Http
import Internal.Syntax exposing (Token, Syntax)
import Vim.AST exposing (AST)
import Internal.Jumps exposing (Location)
import Internal.TextBuffer as B exposing (Patch)
import Model exposing (Key, LintError, BufferInfo, Flags)


type alias BufferIdentifier =
    -- (path, version)
    ( String, Int )


type TokenizeResponse
    = TokenizeSuccess Int Syntax
    | LineTokenizeSuccess Int (List Token)
    | TokenizeCacheMiss -- happens when server restart
    | TokenizeError String


type alias TokenizeRequest =
    { path : String
    , version : Int
    , line : Int
    , lines : String
    }


type Msg
    = PressKey Key -- buffer id, key
    | Resize Size
    | Read (Result Http.Error BufferInfo)
    | Write (Result Http.Error (List Patch))
    | ReadClipboard (Result Http.Error ( Bool, Key, AST, String ))
    | WriteClipboard (Result Http.Error ())
    | SendLint
    | SendTokenize
    | Lint BufferIdentifier (Result String (List LintError))
    | Tokenized BufferIdentifier (Result Http.Error TokenizeResponse)
    | ListFiles (Result String (List String))
    | ReadTags (Result String Location)
    | SearchResult (Result Http.Error String)
    | SetCwd (Result Http.Error String)
    | Boot (Result Http.Error Flags)
    | MouseWheel Int
    | NoneMessage
