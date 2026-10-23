module Shared.Backer.Types where

import Prelude

import Shared.Modal (Modal)

type BackerModel =
      { visible ∷ Boolean
      }

data BackerMessage
      = ToggleVisibility Modal