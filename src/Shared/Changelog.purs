module Shared.Changelog where

import Data.Maybe (Maybe)

type Changelog =
      { id ∷ Int
      , changed ∷ Maybe Int
      , description ∷ String
      , read ∷ Boolean
      }