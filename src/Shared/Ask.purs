module Shared.Ask where

import Data.Maybe (Maybe)
import Shared.DateTime (DateTimeWrapper)
import Shared.Content (Content)

type Ask =
      { id ∷ Int
      , asker :: String
      , question ∷ String
      , answer :: String
      }
