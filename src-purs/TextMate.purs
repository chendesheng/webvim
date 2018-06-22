module TextMate where

import Effect (Effect)

-- tokenize keeps a cache inside it, so it is not pure
foreign import tokenize
  :: String -- path
  -> Int -- line
  -> String -- input string
  -> Effect String

foreign import themeCss :: String
