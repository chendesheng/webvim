module Main where

import Prelude (Unit, bind, discard, pure, show, unit
  ,void, ($), (<>), (*), (#))
import Node.HTTP (Request, Response, createServer, listen, requestMethod
  ,requestURL, setHeader, setStatusCode, setStatusMessage
  ,requestAsStream, responseAsStream)
import File
  (listFiles
  , listDirectory
  , readFile
  , readTags
  , searchFiles
  , writeFile
  , cd
  , makeDir
  )
import Clipboard
    ( readClipboard
    , writeClipboard
    )
import Lint (lint, lintOnTheFly)
import MyRouting (MyRoutes(..), matchUrl, DynamicActions(..))
import Helper
    ( affExit
    , affLog, endResponse
    , setNoCacheHeaders, affWriteString
    , affReadAllString
    , affEnd
    , setCacheSeconds
    , homedir
    , boot
    , argv
    , getMime
    )
import TextMate (affTokenize, affLoadTheme)

import Data.Array as Array
import Data.Int as Int
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Aff (attempt, launchAff_, Aff)
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.String as String

handler :: Request -> Response -> Aff Unit
handler req resp = do
  let
    method = requestMethod req
    url = requestURL req
  affLog $ method <> " " <> url
  case matchUrl method url of
    Right input -> 
      case input of
        StaticFile path -> do
          setCacheSeconds (24*60*60) resp
          readFile true resp path

        DynamicAction action -> do
          setNoCacheHeaders resp
          dynamicActionHandler req resp action

    Left err -> do
      affLog err
      invalidRequest resp



dynamicActionHandler :: Request -> Response -> DynamicActions -> Aff Unit
dynamicActionHandler req resp action = do
  case action of
    ReadFile (NonEmptyString path) ->
      readFile false resp path
        
    WriteFile (NonEmptyString path) ->
      writeFile req resp path 

    ListFiles (NonEmptyString cwd) -> 
      listFiles resp cwd

    ListDirectory (NonEmptyString cwd) -> 
      listDirectory resp cwd

    Search (NonEmptyString cwd) (NonEmptyString s) ->
      searchFiles resp cwd s

    ReadClipboard -> do
      readClipboard resp

    WriteClipboard ->
      writeClipboard req resp

    Lint (NonEmptyString path) ->
      lint req resp path

    LintOnTheFly (NonEmptyString path) ->
      lintOnTheFly req resp path

    ReadTags (NonEmptyString cwd) (NonEmptyString name) ->
      readTags resp cwd name

    Tokenize (NonEmptyString path) line -> do
      liftEffect $ setHeader resp "Content-Type" (getMime "json")
      let outputStream = responseAsStream resp
          inputStream = requestAsStream req
      payload <- affReadAllString inputStream
      json <- affTokenize path line payload
      affWriteString outputStream json
      affEnd outputStream

    Css (NonEmptyString label)  -> do
      liftEffect $ setHeader resp "Content-Type" (getMime "css")
      let outputStream = responseAsStream resp
      css <- affLoadTheme label
      affWriteString outputStream css 
      affEnd outputStream

    Cd (Just (NonEmptyString cwd)) ->
      cd resp cwd

    Cd Nothing -> do
      let outputStream = responseAsStream resp
      affWriteString outputStream homedir
      affEnd outputStream

    Kill -> do
      affLog "bye"
      affExit 0
      liftEffect $ endResponse resp

    Log ->
      -- TODO
      invalidRequest resp

    Boot -> do
      let outputStream = responseAsStream resp
      affWriteString outputStream boot
      affEnd outputStream

    MkDir (NonEmptyString path) -> do
      makeDir resp path

invalidRequest :: Response -> Aff Unit
invalidRequest resp = do
  let outputStream = responseAsStream resp
  liftEffect $ do
    setStatusMessage resp "Invalid Request"
    setStatusCode resp 400
  affEnd outputStream

getPort :: Array String -> Maybe Int
getPort args =
  do
    s <- args
          # Array.mapMaybe (String.stripPrefix $ String.Pattern "--port=")
          # Array.head
    Int.fromString s


main :: Effect Unit
main = do
  let port = argv # getPort # fromMaybe 8899
  server <- createServer
    (\req resp -> launchAff_ $ do
      liftEffect $ setHeader resp "Access-Control-Allow-Origin" "*"
      result <- attempt $ handler req resp
      case result of
        Left err -> liftEffect $ do
          log $ show err
          setStatusCode resp 500
          setStatusMessage resp "Internal Error"
          endResponse resp
        _ ->
          pure unit
    )
  listen server
    { hostname : "0.0.0.0"
    , port : port
    , backlog : Nothing
    }
    $ void do
      log $ "Listening on port " <> show port

