module Update.Message exposing (BufferIdentifier, Msg(..), TokenizeRequest, TokenizeResponse(..))

import Font exposing (FontInfo)
import Http
import Ime exposing (IMEMsg)
import Internal.Jumps exposing (Location)
import Internal.Syntax exposing (Syntax, Token)
import Internal.TextBuffer as B exposing (Patch)
import Internal.Window exposing (Path)
import Model exposing (Buffer, Flags, Key, LintError, Size)
import Result
import Vim.AST exposing (AST)


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


type Msg
    = PressKeys Key -- buffer id, key
    | IMEMessage IMEMsg
    | Resize Size
    | Read (Result Http.Error ( Bool, Buffer )) -- setActive & buffer
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
    | PersistentAll
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
    | MeasureFont FontInfo
    | MouseWheel Path Int Int
    | FocusIme
    | NoneMessage
