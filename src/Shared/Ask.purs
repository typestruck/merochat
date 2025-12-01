module Shared.Ask where

import Data.Maybe (Maybe)
import Shared.Content (Content)
import Shared.DateTime (DateTimeWrapper(..))

type Ask =
      { id ∷ Int
      , asker :: Int
      , name :: String
      , question ∷ String
      , answer :: Maybe String
      }
