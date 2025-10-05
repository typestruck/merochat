module Server.Experiments.Database.Flat where

import Prelude

type FlatQuestion =
      { questionId ∷ Int
      , choiceId ∷ Int
      , question ∷ String
      , choice ∷ String
      , chosen ∷ Boolean
      }