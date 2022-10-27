module Shared.Markdown  where

type Token =
      { "type" ∷ String
      , raw ∷ String
      }

foreign import parseRestricted ∷ String → String
foreign import parse ∷ String → String

foreign import lexer ∷ String → Array Token