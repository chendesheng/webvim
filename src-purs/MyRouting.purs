module MyRouting(MyRoutes(..), matchUrl, DynamicActions(..)) where

import Prelude
import Data.Array (fromFoldable, (:))
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Int (fromString)
import Data.List (List)
import Data.String (joinWith)
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.Maybe (Maybe, maybe)
import Routing (match)
import Routing.Match (Match, eitherMatch, end, list, lit, optionalMatch, params, param, root, str)
import Node.Path (normalize, sep)

data MyRoutes
  = StaticFile String
  | DynamicAction DynamicActions

data DynamicActions
  = ReadFile NonEmptyString
  | WriteFile NonEmptyString
  | ListFiles NonEmptyString
  | Search NonEmptyString NonEmptyString
  | ReadClipboard
  | WriteClipboard
  | Lint NonEmptyString
  | LintOnTheFly NonEmptyString
  | ReadTags NonEmptyString NonEmptyString
  | Tokenize NonEmptyString Int
  | Kill
  | Log
  | Cd (Maybe NonEmptyString)
  | Css NonEmptyString
  | Boot


nonemptyParam :: String -> Match NonEmptyString
nonemptyParam s =
  eitherMatch $ (((maybe (Left "Empty string") Right) <<< NES.fromString) <$> param s)

paramInt :: String -> Match Int
paramInt s =
  eitherMatch $ (((maybe (Left "Not Int") Right) <<< fromString) <$> param s)

paramCwd :: Match NonEmptyString
paramCwd = (NES.toString >>> normalize >>> NonEmptyString) <$> nonemptyParam "cwd"

paramPath :: Match NonEmptyString
paramPath = (NES.toString >>> normalize >>> NonEmptyString) <$> nonemptyParam "path"

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
  [ "dist/webvim.html" <$ (root *> end)
  , "dist/webvim.html" <$ (root *> params *> end)
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
  , ListFiles <$> (root *> lit "ls" *> paramCwd)
  , Search <$> (root *> lit "search" *> paramCwd) <*> (nonemptyParam "s")
  , ReadClipboard <$ (root *> lit "clipboard")
  , Lint <$> (root *> lit "lint" *> paramPath)
  , ReadTags <$> (root *> lit "readtags" *> paramCwd) <*> (nonemptyParam "name")
  , Kill <$ (root *> lit "kill" *> end)
  , Log <$ (root *> lit "log" *> end)
  , Cd <$> (root *> lit "cd" *> optionalMatch paramCwd)
  , Css <$> (root *> lit "css" *> (nonemptyParam "theme"))
  , Boot <$ (root *> lit "boot" *> end)
  ]

routingDynamicPost :: Match DynamicActions
routingDynamicPost = oneOf
  [ WriteFile <$> (root *> lit "write" *> paramPath) 
  , WriteClipboard <$ (root *> lit "clipboard")
  , LintOnTheFly <$> (root *> lit "lint" *> paramPath)
  , Tokenize <$> (root *> lit "tokenize" *> paramPath)
             <*> (paramInt "line")
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
