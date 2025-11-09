module Shared.Backer.Types where

import Prelude

import Shared.Modal (ScreenModal)

type BackerModel =
      { visible ∷ Boolean
      }

data BackerMessage =
      ToggleVisibility ScreenModal