module Shared.Change where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as DS
import Flame.Html.Event as FHE
import Flame.Types (NodeData)

onChange ∷ ∀ m. (String → m) → NodeData m
onChange = FHE.onInput

toMaybe ∷ String → Maybe String
toMaybe s = if DS.null trimmed then Nothing else Just trimmed
      where
      trimmed = DS.trim s
