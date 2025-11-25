module Shared.Options.PaperPlane where

import Prelude

maxMessageCharacters ∷ Int
maxMessageCharacters = 300

maxPaperPlanes ∷ Int
maxPaperPlanes = 7

message :: String -> String
message plane = "I caught your paper plane that says: " <> plane