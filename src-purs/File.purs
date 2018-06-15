module File where

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Helper
    (affWriteBuffer
    , endStream
    , affLog
    , affPipe
    , affWaitEnd
    , affEnd
    , writeStdout
    )
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.HTTP
  (Request
  , requestAsStream
  , Response
  , responseAsStream
  , setStatusCode
  )
import Node.Stream (pipe, writeString)
import Prelude
import Shell (execAsync)
import Data.String (Pattern(..), stripSuffix)


readFile :: Response -> String -> Aff Unit
readFile resp path = do
  affLog ("readFile: " <> path)
  exists <- (FS.exists path)
  liftEffect $
    if exists then
      do
        let outputStream = responseAsStream resp
        fileStream <- createReadStream path 
        void $ pipe fileStream outputStream
    else
      do
        setStatusCode resp 404
        let outputStream = responseAsStream resp
        void $ writeString outputStream UTF8 path $ endStream outputStream
    

writeFile :: Request -> Response -> String -> Aff Unit
writeFile req resp path = do
  let inputStream = requestAsStream req
      outputStream = responseAsStream resp
  fileStream <- liftEffect $ createWriteStream path
  case stripSuffix (Pattern ".elm") path of
    Just _ -> do
      affLog ("elm-format :" <> path)
      result <- execAsync Nothing "elm-format --stdin" (Just inputStream)
      --affLog (show result.error)
      case result.error of
        Just err -> do
          -- FIXME: when input stream is interruptted in the middle of writing,
          -- we might end up saving only part of the file
          affPipe inputStream fileStream
          affEnd outputStream
        _ -> do
          affWriteBuffer outputStream result.stdout
          affWriteBuffer fileStream result.stdout
          affEnd outputStream
    _ -> do
      affPipe inputStream fileStream
      affWaitEnd inputStream
      affEnd outputStream



listFiles :: Response  -> Maybe String -> Aff Unit
listFiles resp cwd = do
  affLog ("listFiles: " <> show cwd)
  let outputStream = responseAsStream resp
  result <- execAsync cwd "ag -l --nocolor" Nothing 
  writeStdout outputStream result


searchFiles :: Response -> Maybe String -> String -> Aff Unit
searchFiles resp cwd s = do
  affLog ("searchFiles: " <> s <> " in " <> show cwd)
  let outputStream = responseAsStream resp
  result <- execAsync cwd ("ag --nocolor --vimgrep " <> s) Nothing
  writeStdout outputStream result


readTags :: Response -> String -> Aff Unit
readTags resp name = do
  affLog ("readTags: " <> name)
  let outputStream = responseAsStream resp
  result <- execAsync Nothing ("readtags -en " <> name) Nothing
  writeStdout outputStream result


