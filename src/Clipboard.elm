module Clipboard exposing (read, write)

import Helper.Helper exposing (httpErrorMessage)
import Http
import Json.Decode as Decode
import Model exposing (Key)
import Update.Message exposing (Msg(..))
import Vim.AST exposing (AST)


read : String -> Cmd (Result String String)
read url =
    Http.get
        { url = url ++ "/clipboard"
        , expect = Http.expectString (Result.mapError httpErrorMessage)
        }


write : String -> String -> Cmd (Result String ())
write url str =
    Http.post
        { url = url ++ "/clipboard"
        , body = Http.stringBody "text/plain" str
        , expect =
            Http.expectJson
                (Result.mapError httpErrorMessage)
                (Decode.succeed ())
        }
