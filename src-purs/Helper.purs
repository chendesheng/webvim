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
  (write, end, pipe
  , Writable, Readable
  , writeString, onEnd
  )
import Node.Process (exit)
import Node.Encoding (Encoding(..))
import Node.Buffer (Buffer)
import Prelude

foreign import readAllString
  :: Readable ()
  -> (String -> Effect Unit)
  -> Effect Unit

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

affWriteBuffer :: Writable () -> Buffer -> Aff Unit
affWriteBuffer stream buffer =
  makeAff (\callback -> do
    void $ write stream buffer (callback $ Right unit)
    pure nonCanceler
  )

endStream :: Writable () -> Effect Unit
endStream stream = end stream (pure unit)

endResponse :: Response -> Effect Unit 
endResponse resp = endStream $ responseAsStream resp

affEnd :: Writable () -> Aff Unit
affEnd stream = liftEffect $ endStream stream

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

writeStdout :: Writable () -> ExecResult -> Aff Unit
writeStdout outputStream result =
  case result.error of
    Just err ->
      affEnd outputStream
    _ -> do
      affWriteBuffer outputStream result.stdout
      affEnd outputStream

