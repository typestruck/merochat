module Shared.Change where

import Prelude

import Client.Dom as CCD
import Data.Maybe (Maybe(..))
import Data.String as DS
import Flame.Html.Event as HA
import Flame.Types (NodeData)
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.Event.Event as WEE

onChange ∷ ∀ m. (String → m) → NodeData m
onChange message = HA.createRawEvent "change" handler
      where
      handler event = do
            value ← CCD.value $ SU.fromJust do
                  target ← WEE.target event
                  WDE.fromEventTarget target
            pure <<< Just $ message value

toMaybe ∷ String → Maybe String
toMaybe s = if DS.null trimmed then Nothing else Just trimmed
      where
      trimmed = DS.trim s