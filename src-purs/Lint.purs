module Lint (lint, lintOnTheFly) where

import Prelude (Unit, bind, discard, pure, show, void, ($), (<>), (==), (/=), (#), map)
import Effect.Aff (Aff)
import Helper
  ( affLog
  , affWriteString
  , affWriteStdout
  , affBufferToString
  , affEnd
  , tempdir
  , currentdir
  , isWindows
  )
import Node.HTTP (Request, Response, requestAsStream, responseAsStream)
import Shell (execAsync)
import Data.Maybe (Maybe(..), fromMaybe)
import Node.FS.Stream (createWriteStream)
import Node.Stream (pipe)
import Effect.Class (liftEffect)
import Node.Path as Path
import Node.FS.Aff as FS
import Data.String as Str
import Data.String.CodePoints (codePointFromChar)

tempfile :: String
tempfile =
    Path.concat [tempdir, "912ec803b2ce49e4a541068d495ab570.elm"]


findProjectDir :: String -> String -> Aff (Maybe String)
findProjectDir projectFile path = do
    let dir = Path.dirname path
        file = Path.concat [dir, projectFile]

    exists <- (FS.exists file)
    if exists then
        pure $ Just dir
        else if dir == path then
            pure Nothing
        else
            findProjectDir projectFile dir

elmLint :: Request -> Response -> String -> Aff Unit
elmLint req resp path = do
  let inputStream = requestAsStream req
      outputStream = responseAsStream resp
  absolutePath <- FS.realpath $ Path.normalize path
  cwd <- findProjectDir "elm.json" absolutePath
  void $ affLog ("elmLint: " <> absolutePath <> " in " <> show cwd)
  let cmdName = if isElmTestsModule (fromMaybe "" cwd) absolutePath
                  then "elm-test"
                  else "elm"
  result <- execAsync cwd (cmdName <> " make "
                          <> absolutePath
                          <> " --report=json --output=/dev/null"
                          )
                          Nothing
  stdoutStr <- affBufferToString result.stdout
  stderrStr <- affBufferToString result.stderr
  affWriteString outputStream (fromMaybe "" cwd <> "\n"
                              <> stdoutStr <> stderrStr
                              )
  affEnd outputStream


eslint :: Request -> Response -> String -> Aff Unit
eslint req resp path = do
  let inputStream = requestAsStream req
      outputStream = responseAsStream resp
      formatter = [currentdir, "../eslint-json-formatter.js"]
                  # Path.concat 
                  # Path.normalize
  absolutePath <- FS.realpath $ Path.normalize path
  cwd <- findProjectDir "package.json" absolutePath
  let eslintCmd = if isWindows
                    then "node_modules\\.bin\\eslint.cmd"
                    else "./node_modules/.bin/eslint"
  affLog formatter
  affLog "eslint"
  result <- execAsync
              cwd
              (Str.joinWith 
                " "
                [ eslintCmd
                , "--format=" <> formatter
                , path
                ]
              )
              (Just inputStream)
  affWriteStdout outputStream result

isElmTestsModule :: String -> String -> Boolean
isElmTestsModule cwd path =
  path
    # Path.relative (cwd <> Path.sep <> "tests") 
    # Str.codePointAt 0
    # map ((/=) (codePointFromChar '.'))
    # fromMaybe false

elmLintOnTheFly :: Request -> Response -> String -> Aff Unit
elmLintOnTheFly req resp path = do
  let inputStream = requestAsStream req
      outputStream = responseAsStream resp
  absolutePath <- FS.realpath $ Path.normalize path
  cwd <- findProjectDir "elm.json" absolutePath
  void $ affLog ("elmLintOnTheFly: " <> absolutePath <> " in " <> show cwd)
  liftEffect $ do
      fileStream <- createWriteStream tempfile
      void $ pipe inputStream fileStream
  let cmdName = if isElmTestsModule (fromMaybe "" cwd) absolutePath
                  then "elm-test"
                  else "elm"
  result <- execAsync cwd (cmdName <> " make "
                          <> tempfile
                          <> " --report=json --output=/dev/null"
                          )
                          Nothing
  stdoutStr <- affBufferToString result.stdout
  stderrStr <- affBufferToString result.stderr
  affWriteString outputStream (fromMaybe "" cwd <> "\n"
                              <> stdoutStr <> stderrStr
                              )
  affEnd outputStream

eslintOnTheFly :: Request -> Response -> String -> Aff Unit
eslintOnTheFly req resp path = do
  let inputStream = requestAsStream req
      outputStream = responseAsStream resp
      formatter = [currentdir, "../eslint-json-formatter.js"]
                  # Path.concat 
                  # Path.normalize
  absolutePath <- FS.realpath $ Path.normalize path
  cwd <- findProjectDir "package.json" absolutePath
  let eslintCmd = if isWindows
                    then "node_modules\\.bin\\eslint.cmd"
                    else "./node_modules/.bin/eslint"
  affLog formatter
  affLog eslintCmd
  affLog "eslintOnTheFly"
  result <- execAsync
              cwd
              (Str.joinWith 
                " "
                [ eslintCmd
                , "--stdin"
                , "--stdin-filename=" <> path
                , "--format=" <> formatter
                ]
              )
              (Just inputStream)
  affWriteStdout outputStream result


lint :: Request -> Response -> String -> Aff Unit
lint req resp path = do
  affLog ("lint: " <> path)
  let ext = Path.extname path
  if ext == ".elm" then
    elmLint req resp path
    else eslint req resp path


lintOnTheFly :: Request -> Response -> String -> Aff Unit
lintOnTheFly req resp path = do
  affLog ("lintOnTheFly: " <> path)
  let ext = Path.extname path
  if ext == ".elm" then
    elmLintOnTheFly req resp path
    else eslintOnTheFly req resp path
