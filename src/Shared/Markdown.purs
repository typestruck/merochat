module Shared.Markdown  where

import Data.Nullable(Nullable(..))

newtype Token = Token
      { "type" ∷ String
      , raw ∷ String
      , tokens :: Nullable (Array Token)
      }

foreign import parseRestricted ∷ String → String
foreign import parse ∷ String → String

foreign import lexer ∷ String → Array Token