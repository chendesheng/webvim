module Lint (lint, lintOnTheFly) where

import Prelude (Unit, bind, discard, pure, show, void, ($), (<>), (==), (#))
import Effect.Aff (Aff)
import Helper (affLog, affWriteString, affEnd, tempdir, currentdir)
import Node.HTTP (Request, Response, requestAsStream, responseAsStream)
import Node.Buffer (toString)
import Node.Encoding (Encoding(..))
import Shell (execAsync)
import Data.Maybe (Maybe(..), fromMaybe)
import Node.FS.Stream (createWriteStream)
import Node.Stream (pipe)
import Effect.Class (liftEffect)
import Node.Path as Path
import Node.FS.Aff as FS
import Data.String as Str

tempfile :: String
tempfile =
    Path.concat [tempdir, "912ec803b2ce49e4a541068d495ab570.txt"]


findProjectDir :: String -> Aff (Maybe String)
findProjectDir path = do
    let dir = Path.dirname path
        file = Path.concat [dir, "elm-package.json"]

    exists <- (FS.exists file)
    if exists then
        pure $ Just dir
        else if dir == path then
            pure Nothing
        else
            findProjectDir dir

elmLint :: Request -> Response -> String -> Aff Unit
elmLint req resp path = do
  let inputStream = requestAsStream req
      outputStream = responseAsStream resp
  absolutePath <- FS.realpath $ Path.normalize path
  cwd <- findProjectDir absolutePath
  void $ affLog ("elmLint: " <> absolutePath <> " in " <> show cwd)
  result <- execAsync cwd ("elm-make "
                          <> absolutePath
                          <> "  --yes --warn --report=json --output=/dev/null"
                          )
                          Nothing
  stdoutStr <- liftEffect $ toString UTF8 result.stdout
  stderrStr <- liftEffect $ toString UTF8 result.stderr
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
  affLog formatter

  void $ affLog "eslintOnTheFly"
  result <- execAsync
              Nothing
              (Str.joinWith 
                " "
                [ "eslint"
                , "--format=" <> formatter
                , "\"" <> path <> "\""
                ]
              )
              (Just inputStream)
  stdoutStr <- liftEffect $ toString UTF8 result.stdout
  affWriteString outputStream stdoutStr
  affEnd outputStream


elmLintOnTheFly :: Request -> Response -> String -> Aff Unit
elmLintOnTheFly req resp path = do
  let inputStream = requestAsStream req
      outputStream = responseAsStream resp
  absolutePath <- FS.realpath $ Path.normalize path
  cwd <- findProjectDir $ absolutePath
  void $ affLog ("elmLintOnTheFly: " <> absolutePath <> " in " <> show cwd)
  liftEffect $ do
      fileStream <- createWriteStream tempfile
      void $ pipe inputStream fileStream
  result <- execAsync cwd ("elm-make "
                          <> tempfile
                          <> "  --yes --warn --report=json --output=/dev/null"
                          )
                          Nothing
  stdoutStr <- liftEffect $ toString UTF8 result.stdout
  stderrStr <- liftEffect $ toString UTF8 result.stderr
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
  affLog formatter

  void $ affLog "eslintOnTheFly"
  result <- execAsync
              Nothing
              (Str.joinWith 
                " "
                [ "eslint"
                , "--stdin"
                , "--stdin-filename=" <> path
                , "--format=" <> formatter
                ]
              )
              (Just inputStream)
  stdoutStr <- liftEffect $ toString UTF8 result.stdout
  affWriteString outputStream stdoutStr
  affEnd outputStream


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
