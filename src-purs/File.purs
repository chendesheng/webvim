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
import Node.Stream (pipe, writeString)
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
      -- hold input in memory and use later if format failed
      input <- affReadAllString inputStream
      result <- execAsync Nothing "elm-format --stdin"
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
    _ -> do
      affPipe inputStream fileStream
      affWriteString outputStream "[]"
      affWaitEnd inputStream
  affEnd outputStream



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


