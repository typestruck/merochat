module Shared.Keydown where

import Prelude

import Client.Common.Dom as CCD
import Data.Maybe (Maybe(..))
import Flame (Key)
import Flame.Html.Event as HA
import Flame.Types (NodeData)
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.Event.Event (Event)
import Web.Event.Event as WEE
import Web.UIEvent.KeyboardEvent as WUK

keyDownOn ∷ ∀ m. Key → (Event → m) → NodeData m
keyDownOn keyName message = HA.createRawEvent "keydown" handler
      where
      handler event = do
            let
                  keyboardEvent = SU.fromJust $ WUK.fromEvent event
                  key = WUK.key keyboardEvent
            if key == keyName && not WUK.shiftKey keyboardEvent then
                  pure <<< Just $ message event
            else
                  pure Nothing

onEnter ∷ ∀ m. (String → m) → NodeData m
onEnter message = HA.createRawEvent "keydown" handler
      where
      handler event = do
            let key = WUK.key <<< SU.fromJust $ WUK.fromEvent event
            if key == "Enter" then do
                  let
                        element = SU.fromJust do
                              target ← WEE.target event
                              WDE.fromEventTarget target
                  v ← CCD.value element
                  CCD.setValue element ""
                  pure <<< Just $ message v
            else
                  pure Nothing
