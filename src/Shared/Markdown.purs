module Shared.Markdown where

import Data.Nullable (Nullable(..))

newtype Token = Token
      { "type" ∷ String
      , raw ∷ String
      , text ∷ String
      , tokens ∷ Nullable (Array Token)
      }

foreign import parseRestricted ∷ String → String

--second parameter is a wrapper function for event values
foreign import parse ∷ ∀ v w. String → (v → w) → String

foreign import lexer ∷ String → Array Token