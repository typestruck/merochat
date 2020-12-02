module Shared.Markdown(parse, parseRestricted) where

foreign import parseRestricted :: String -> String
foreign import parse :: String -> String
