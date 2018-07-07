module Clipboard (readClipboard, writeClipboard) where

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Helper (affLog, affWriteString, affEnd, affReadAllString)
import Helper as H
import Node.HTTP
  (Request
  , requestAsStream
  , Response
  , responseAsStream
  )
import Prelude
 
readClipboard :: Response -> Aff Unit
readClipboard resp = do
  affLog "readClipboard"
  let outputStream = responseAsStream resp
  s <- liftEffect $ H.readClipboard
  affWriteString outputStream s
  affEnd outputStream


writeClipboard :: Request -> Response -> Aff Unit
writeClipboard req resp = do
  affLog "writeClipboard"
  let inputStream = requestAsStream req
      outputStream = responseAsStream resp
  s <- affReadAllString inputStream
  void $ liftEffect $ H.writeClipboard s
  affEnd outputStream
