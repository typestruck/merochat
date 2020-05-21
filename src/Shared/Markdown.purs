module Shared.Markdown where

import Prelude

foreign import parse :: String -> String
foreign import sanitize :: String -> String

toHTML :: String -> String
toHTML = sanitize <<< parse