module File where

import Data.Maybe (Maybe(..))
import Data.String.Regex as Re
import Data.String.Regex.Flags (noFlags)
import Data.Either(Either(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Helper
    ( endStream
    , affLog
    , affPipe
    , affWaitEnd
    , affEnd
    , affWriteStdout
    , affReadAllString
    , affWriteString
    , affBufferToString
    , createReadableStream
    , diff
    , homedir
    , isWindows
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
  , setHeader
  )
import Node.Stream
  (pipe
  , Writable
  , Readable
  , writeString
  )
import Node.Process as Process
import Prelude
import Shell (execAsync)
import Data.String (Pattern(..), stripSuffix)
import Node.FS.Stats (isDirectory)


readFile :: Response -> String -> Aff Unit
readFile resp path = do
  affLog ("readFile: " <> path)
  exists <- (FS.exists path)
  liftEffect $
    if exists then
      do
        let outputStream = responseAsStream resp
        setHeader resp "Path" path
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
      format inputStream outputStream fileStream "elm-format --stdin"
    _ ->
      case stripSuffix (Pattern ".js") path of
        Just _ -> do
          let cmd = if isWindows then "jsfmt.cmd" else "jsfmt"
          affLog ("jsformat :" <> path)
          format inputStream outputStream fileStream cmd
        _ -> do
          affPipe inputStream fileStream
          affWaitEnd inputStream
          affWriteString outputStream "[]"
  affEnd outputStream


format :: Readable () -> Writable () -> Writable() -> String -> Aff Unit
format inputStream outputStream fileStream cmd = do
  input <- affReadAllString inputStream
  result <- execAsync Nothing cmd
              (Just $ createReadableStream input)
  affLog (show result.error)
  case result.error of
    Just err -> do
      affWriteString outputStream "[]"
      affWriteString fileStream input
    _ -> do
      formatted <- affBufferToString result.stdout
      affWriteString fileStream formatted
      --affLog $ "formatted" <> formatted
      --affLog $ diff input formatted
      affWriteString outputStream $ diff input formatted

listFiles :: Response -> String -> Aff Unit
listFiles resp cwd = do
  affLog ("listFiles: " <> show cwd)
  let outputStream = responseAsStream resp
  result <- execAsync (Just cwd) "git ls-files" Nothing
  affWriteStdout outputStream result


searchFiles :: Response -> String -> String -> Aff Unit
searchFiles resp cwd s = do
  affLog ("searchFiles: " <> s <> " in " <> show cwd)
  let outputStream = responseAsStream resp
  result <- execAsync (Just cwd) ("ag --nocolor --vimgrep " <> s) Nothing
  affWriteStdout outputStream result


readTags :: Response -> String -> String -> Aff Unit
readTags resp cwd name = do
  affLog ("readTags: " <> name)
  let outputStream = responseAsStream resp
  result <- execAsync (Just cwd) ("readtags -en " <> name) Nothing
  affWriteStdout outputStream result

cd :: Response -> String -> Aff Unit
cd resp cwd = do
  let outputStream = responseAsStream resp
      cwd' = case Re.regex "^~" noFlags of
               Right re -> Re.replace re homedir cwd
               _ -> cwd
  affLog ("cd: " <> cwd')
  affLog ("home: " <> homedir)
  exists <- FS.exists cwd'
  s <- (if exists then do
          stat <- FS.stat cwd'
          if isDirectory stat then
            FS.realpath cwd'
            else
              liftEffect $ Process.cwd
          else
            liftEffect $ Process.cwd)
  affWriteString outputStream s
  affEnd outputStream


