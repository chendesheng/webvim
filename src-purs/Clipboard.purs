module Clipboard (readClipboard, writeClipboard) where

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Helper (affLog, writeStdout)
import Node.HTTP
  (Request
  , requestAsStream
  , Response
  , responseAsStream
  )
import Prelude
import Shell (execAsync)
 
readClipboard :: Response -> Aff Unit
readClipboard resp = do
  affLog "readClipboard"
  let outputStream = responseAsStream resp
  result <- execAsync Nothing "pbpaste" Nothing
  writeStdout outputStream result


writeClipboard :: Request -> Aff Unit
writeClipboard req = do
  affLog "writeClipboard"
  let inputStream = requestAsStream req
  void $ execAsync Nothing "pbcopy" (Just inputStream)
