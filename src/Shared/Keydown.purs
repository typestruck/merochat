module Shared.Keydown where

import Prelude

import Client.Ui.Event as CUEV
import Data.Maybe (Maybe(..))
import Flame (Key)
import Flame.Html.Event (Event)
import Flame.Html.Event as FHE
import Flame.Types (NodeData)

keyHandler ∷ ∀ m. Key → (Event → m) → Event → Maybe m
keyHandler keyName message event =
      if CUEV.key event == keyName && not (CUEV.shiftKey event) then
            Just $ message event
      else
            Nothing

keyDownOn ∷ ∀ m. Key → (Event → m) → NodeData m
keyDownOn keyName message = FHE.createRawEvent "bindkeydown" $ pure <<< keyHandler keyName message

onEnter ∷ ∀ m. (String → m) → NodeData m
onEnter = FHE.onConfirm
