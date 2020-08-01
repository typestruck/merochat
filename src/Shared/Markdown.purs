module Shared.Markdown where

import Prelude

foreign import parseRestricted :: String -> String
foreign import parse :: String -> String

toHTML :: String -> String
toHTML = parse

toRestrictedHTML :: String -> String
toRestrictedHTML = parseRestricted