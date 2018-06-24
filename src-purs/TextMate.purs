module TextMate where

import Effect (Effect)
import Effect.Aff (Aff, nonCanceler, makeAff)
import Prelude (Unit, discard, ($), pure)
import Data.Either (Either(..))

-- tokenize keeps a cache inside it, so it is not pure
foreign import tokenize
  :: String -- path
  -> Int -- line
  -> String -- input string
  -> (String -> Effect Unit)
  -> Effect Unit


affTokenize :: String -> Int -> String -> Aff String
affTokenize path line payload =
  makeAff (\callback -> do
    tokenize path line payload (\s -> callback $ Right s)
    pure nonCanceler
  )

foreign import loadTheme
  :: String -- theme label; use empty string if not exists
  -> (String -> Effect Unit)
  -> Effect Unit

affLoadTheme :: String -> Aff String
affLoadTheme label =
  makeAff (\callback -> do
    loadTheme label (\s -> callback $ Right s)
    pure nonCanceler
  )

