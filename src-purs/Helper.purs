module Helper where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, nonCanceler, makeAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Node.ChildProcess (ExecResult)
import Node.HTTP (Response, responseAsStream, setHeader)
import Node.Stream
  (end, pipe
  , Writable, Readable
  , writeString, onEnd
  )
import Node.Process (exit)
import Node.Encoding (Encoding(..))
import Node.Buffer (Buffer, toString)
import Prelude

foreign import readAllString
  :: Readable ()
  -> (String -> Effect Unit)
  -> Effect Unit

foreign import createReadableStream :: String -> Readable ()

-- return json stringfied result
foreign import diff :: String -> String -> String

foreign import homedir :: String

foreign import currentdir :: String

foreign import tempdir :: String

foreign import boot :: String

foreign import isWindows :: Boolean

affLog :: String -> Aff Unit
affLog s = liftEffect $ Console.log s

affPipe :: Readable () -> Writable () -> Aff Unit
affPipe s1 s2 = void $ liftEffect $ pipe s1 s2

affExit :: Int -> Aff Unit
affExit code = liftEffect $ exit code

affWriteString :: Writable () -> String -> Aff Unit
affWriteString stream s =
  makeAff (\callback -> do
    void $ writeString stream UTF8 s (callback $ Right unit)
    pure nonCanceler
  )

affReadAllString :: Readable () -> Aff String
affReadAllString stream =
  makeAff (\callback -> do
    readAllString stream (\s -> callback $ Right s)
    pure nonCanceler
  )


affWaitEnd :: Readable () -> Aff Unit
affWaitEnd inputStream =
  makeAff (\callback -> do
    onEnd inputStream (callback $ Right unit)
    pure nonCanceler
  )

endStream :: Writable () -> Effect Unit
endStream stream = end stream (pure unit)

endResponse :: Response -> Effect Unit 
endResponse resp = endStream $ responseAsStream resp

affEnd :: Writable () -> Aff Unit
affEnd stream = liftEffect $ endStream stream

affBufferToString :: Buffer -> Aff String
affBufferToString buffer =
  liftEffect $ toString UTF8 buffer

setCacheSeconds :: Int -> Response -> Aff Unit
setCacheSeconds seconds resp =
  liftEffect $ do
    setHeader resp "Cache-Control" "Public"
    setHeader resp "Cache-Control" ("max-age=" <> show seconds)

setNoCacheHeaders :: Response -> Aff Unit
setNoCacheHeaders resp = liftEffect $ do
  setHeader resp "Cache-Control" "no-cache, no-store, must-revalidate"
  setHeader resp "Pragma" "no-cache"
  setHeader resp "Expires" "0"

affWriteStdout :: Writable () -> ExecResult -> Aff Unit
affWriteStdout outputStream result =
  case result.error of
    Just err ->
      affEnd outputStream
    _ -> do
      str <- affBufferToString result.stdout
      affWriteString outputStream str
      affEnd outputStream

