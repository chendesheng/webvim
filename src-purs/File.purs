module File where

import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.String.Regex as Re
import Data.String.Regex.Flags (noFlags)
import Data.Either(Either(..))
import Effect.Aff (Aff, attempt)
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
    , getMime
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
import Shell (execAsync, execAsyncParams)
import Data.String (Pattern(..), stripSuffix)
--import Data.String.Common (joinWith)
import Node.FS.Stats (isDirectory, modifiedTime)


readFile :: Boolean -> Response -> String -> Aff Unit
readFile isStatic resp path = do
  affLog ("readFile: " <> path)
  result <- attempt $ writeLastModified path resp
  case result of
    Left _ ->
      liftEffect $ do
        setStatusCode resp 404
        let outputStream = responseAsStream resp
        void $ writeString outputStream UTF8 path $ endStream outputStream
    Right _ ->
      do
        stat <- FS.stat path
        if isDirectory stat then
          liftEffect $ do
            setStatusCode resp 500
            let outputStream2 = responseAsStream resp
            void $ writeString outputStream2 UTF8 "Cannot open a directory." $ endStream outputStream2
          else
          liftEffect $ do
            setHeader resp "Content-Type" (getMime (if isStatic then path else "text"))
            let outputStream = responseAsStream resp
            fileStream <- createReadStream path
            void $ pipe fileStream outputStream


writeFile :: Request -> Response -> String -> Aff Unit
writeFile req resp path = do
  let inputStream = requestAsStream req
      outputStream = responseAsStream resp
  fileStream <- liftEffect $ createWriteStream path
  case stripSuffix (Pattern ".elm") path of
    Just _ -> do
      affLog ("elm-format :" <> path)
      format resp path inputStream outputStream fileStream "elm-format --stdin"
    _ ->
      case stripSuffix (Pattern ".js") path of
        Just _ -> do
          let cmd = if isWindows then "jsfmt.cmd" else "jsfmt"
          affLog ("jsformat :" <> path)
          format resp path inputStream outputStream fileStream cmd
        _ -> do
          affPipe inputStream fileStream
          affWaitEnd inputStream
          writeLastModified path resp
          affWriteString outputStream "[]"
    
  affEnd outputStream

writeLastModified :: String -> Response -> Aff Unit
writeLastModified path resp =
  do
    stat <- FS.stat path
    liftEffect $ setHeader resp "last-modified"
      (JSDate.toString $ JSDate.fromDateTime $ modifiedTime stat)


format :: Response -> String -> Readable () -> Writable () -> Writable() -> String -> Aff Unit
format resp path inputStream outputStream fileStream cmd = do
  input <- affReadAllString inputStream
  result <- execAsync Nothing cmd
              (Just $ createReadableStream input)
  affLog (show result.error)
  case result.error of
    Just err -> do
      writeLastModified path resp

      affWriteString outputStream "[]"
      affWriteString fileStream input
    _ -> do
      formatted <- affBufferToString result.stdout
      affWriteString fileStream formatted

      writeLastModified path resp

      --affLog $ "formatted" <> formatted
      --affLog $ diff input formatted
      affWriteString outputStream $ diff input formatted

listFiles :: Response -> String -> Aff Unit
listFiles resp cwd = do
  affLog ("listFiles: " <> show cwd)
  let outputStream = responseAsStream resp
  result <- execAsync (Just cwd) "fd -c never" Nothing
  affWriteStdout outputStream result

listDirectory :: Response -> String -> Aff Unit
listDirectory resp cwd = do
  affLog ("listDirectories: " <> show cwd)
  let outputStream = responseAsStream resp
  result <- execAsync (Just cwd) "ls -pa1" Nothing
  affWriteStdout outputStream result


searchFiles :: Response -> String -> String -> Aff Unit
searchFiles resp cwd s = do
  affLog ("searchFiles: " <> s <> " in " <> show cwd)
  let outputStream = responseAsStream resp
  result <- execAsyncParams (Just cwd) ["rg", "--vimgrep", s] Nothing
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

makeDir :: Response -> String -> Aff Unit
makeDir resp path = do
  affLog ("makePath: " <> show path)
  let outputStream = responseAsStream resp
  _ <- execAsync Nothing ("mkdir -p " <> path) Nothing
  affEnd outputStream


