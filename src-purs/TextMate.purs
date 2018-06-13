module TextMate where

import Effect (Effect)

-- tokenize keeps a cache inside it, so it is not pure
foreign import tokenize
  :: String -- path
  -> Int -- line
  -> Int -- version
  -> String -- input string
  -> Effect String
