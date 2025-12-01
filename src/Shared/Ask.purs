module Shared.Ask where

import Data.Maybe (Maybe)
import Shared.Content (Content)
import Shared.DateTime (DateTimeWrapper(..))

type A r =
      ( id ∷ Int
      , asker ∷ Int
      , name ∷ String
      , question ∷ String
      , answer ∷ Maybe String
      | r
      )

type Ask = Record (A ())