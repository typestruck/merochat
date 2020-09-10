module Shared.Markdown(toHTML, toRestrictedHTML) where

--snabbdom hooks dont work server side so just use innerHTML
foreign import parseRestricted :: String -> String
foreign import parse :: String -> String

toHTML :: String -> String
toHTML = parse

toRestrictedHTML :: String -> String
toRestrictedHTML = parseRestricted