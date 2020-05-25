module Shared.Markdown where

import Prelude

foreign import parseRestricted :: String -> String
foreign import parse :: String -> String
foreign import sanitize :: String -> String

toHTML :: String -> String
toHTML = sanitize <<< parse

toRestrictedHTML :: String -> String
toRestrictedHTML = sanitize <<< parseRestricted