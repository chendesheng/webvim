module MyRouting(MyRoutes(..), matchUrl, DynamicActions(..)) where

import Prelude
import Data.Array (fromFoldable, (:))
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Int (fromString)
import Data.List (List)
import Data.String (joinWith)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Maybe (Maybe, maybe)
import Data.Either (Either(..))
import Node.Path (sep)
import Routing (match)
import Routing.Match (Match, eitherMatch, end, list, lit, optionalMatch, param, root, str)

data MyRoutes
  = StaticFile String
  | DynamicAction DynamicActions

data DynamicActions
  = ReadFile NonEmptyString
  | WriteFile NonEmptyString
  | ListFiles (Maybe NonEmptyString)
  | Search (Maybe NonEmptyString) NonEmptyString
  | ReadClipboard
  | WriteClipboard
  | Lint NonEmptyString
  | LintOnTheFly NonEmptyString
  | ReadTags NonEmptyString
  | Tokenize NonEmptyString Int Int
  | Kill
  | Log


nonemptyParam :: String -> Match NonEmptyString
nonemptyParam s =
  eitherMatch $ (((maybe (Left "Empty string") Right) <<< NES.fromString) <$> param s)

paramInt :: String -> Match Int
paramInt s =
  eitherMatch $ (((maybe (Left "Not Int") Right) <<< fromString) <$> param s)

paramPath :: Match NonEmptyString
paramPath = nonemptyParam "path"

distFolder :: List String -> String
distFolder parts =
  joinWith sep ("dist" : fromFoldable parts)

fontFolder :: List String -> String
fontFolder parts =
  joinWith sep ("node_modules"
                            : "@fortawesome"
                            : "fontawesome-free-webfonts"
                            : "webfonts"
                            : fromFoldable parts)


routingStaticFiles :: Match String
routingStaticFiles = oneOf
  [ "index.html" <$ (root *> end)
  , "index.html" <$ (root *> lit "index.html" *> end)
  , "favicon.ico" <$ (root *> lit "favicon.ico" *> end)
  , distFolder <$> (root *> lit "dist" *> list str)
  , fontFolder <$> (root
                   *> lit "node_modules"
                   *> lit "@fortawesome"
                   *> lit "fontawesome-free-webfonts"
                   *> lit "webfonts"
                   *> list str)
   ]

routingDynamicGet :: Match DynamicActions
routingDynamicGet = oneOf
  [ ReadFile <$> (root *> lit "read" *> paramPath)
  , ListFiles <$> (root *> lit "ls" *> optionalMatch paramPath)
  , Search <$> (root *> lit "search" *> optionalMatch paramPath) <*> (nonemptyParam "s")
  , ReadClipboard <$ (root *> lit "clipboard")
  , Lint <$> (root *> lit "lint" *> paramPath)
  , ReadTags <$> (root *> lit "readtags" *> (nonemptyParam "name"))
  , Kill <$ (root *> lit "kill" *> end)
  , Log <$ (root *> lit "log" *> end)
  ]

routingDynamicPost :: Match DynamicActions
routingDynamicPost = oneOf
  [ WriteFile <$> (root *> lit "write" *> paramPath)
  , WriteClipboard <$ (root *> lit "clipboard")
  , LintOnTheFly <$> (root *> lit "lint" *> paramPath)
  , Tokenize <$> (root *> lit "tokenize" *> paramPath) <*> (paramInt "line") <*> (paramInt "version")
  ]

matchUrl :: String -> String -> Either String MyRoutes
matchUrl method url =
  if method == "GET" then
    match
      (oneOf [ StaticFile <$> routingStaticFiles
             , DynamicAction <$> routingDynamicGet
             ]
      ) url
    else match (DynamicAction <$> routingDynamicPost) url
