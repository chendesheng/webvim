module Update.Message exposing
    ( BufferIdentifier
    , Msg(..)
    , TokenizeRequest
    , TokenizeResponse(..)
    )

import Boot
import Debouncers exposing (DebounceMessage)
import Helper.Debounce as Deb
import Http
import Ime exposing (IMEMsg)
import Internal.Jumps exposing (Location)
import Internal.Syntax exposing (Syntax, Token)
import Internal.TextBuffer exposing (Patch)
import Internal.Window as Win
import Model exposing (Key)
import Model.Buffer exposing (Buffer)
import Model.Lint exposing (LintError)
import Model.Size exposing (Size)
import Vim.AST exposing (AST)


type alias BufferIdentifier =
    -- (bufId, version)
    ( String, Int )


type TokenizeResponse
    = TokenizeSuccess Int Syntax
    | TokenizeLineSuccess Int (List Token)
    | TokenizeCacheMiss -- happens when server restart
    | TokenizeError String


type alias TokenizeRequest =
    { bufId : String
    , path : String
    , version : Int
    , line : Int
    , lines : String
    }


type Msg
    = PressKeys Key -- buffer id, key
    | BootMessage Boot.Message
    | IMEMessage IMEMsg
    | Debounce DebounceMessage
      -- the DebounceMessage here is for finding the debouncer model
    | Debouncing DebounceMessage (Deb.Message DebounceMessage)
    | Resize Size
    | Read (Result String ( Win.Path, Buffer )) -- setActive & buffer
    | Write (Result String ( String, List Patch ))
    | MakeDir (Result String ())
    | ReadClipboard
        (Result String
            { replaying : Bool
            , key : Key
            , ast : AST
            , s : String
            }
        )
    | WriteClipboard (Result String ())
    | Lint BufferIdentifier (Result String (List LintError))
    | Tokenized BufferIdentifier (Result String TokenizeResponse)
    | ListFiles (Result String (List String))
    | ListDirs (Result String (List String))
    | ListAllFiles (Result String (List String)) -- recursive
    | ListBuffers (List String)
    | ReadTags (Result String Location)
    | SearchResult (Result String String)
    | SetCwd (Result String String)
    | MouseWheel Win.Path Int Int
    | MouseClick Win.Path
    | FocusIme
    | NoneMessage
