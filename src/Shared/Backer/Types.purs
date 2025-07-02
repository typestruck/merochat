module Shared.Backer.Types where

import Prelude

import Shared.Modal.Types (ScreenModal)

type BackerModel =
      { visible ∷ Boolean
      }

data BackerMessage =
      ToggleVisibility ScreenModal