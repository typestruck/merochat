module Shared.Keydown where

import Prelude

import Client.Dom as CCD
import Data.Maybe (Maybe(..))
import Flame (Key)
import Flame.Html.Event as HA
import Flame.Subscription.Internal.Create as FS
import Flame.Types (NodeData, Source, Subscription)
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.Event.Event (Event)
import Web.Event.Event as WEE
import Web.UIEvent.KeyboardEvent as WUK

keyHandler ∷ ∀ m. Key → (Event → m) → Event → Maybe m
keyHandler keyName message event =
      let
            keyboardEvent = SU.fromJust $ WUK.fromEvent event
            key = WUK.key keyboardEvent
      in
            if key == keyName && not WUK.shiftKey keyboardEvent then
                  Just $ message event
            else
                  Nothing

keyDownOn ∷ ∀ m. Key → (Event → m) → NodeData m
keyDownOn keyName message = HA.createRawEvent "keydown" (pure <<< keyHandler keyName message)

keyDownOnSubscription ∷ ∀ m. Source → Key → (Event → m) → Subscription m
keyDownOnSubscription source keyName message = FS.createRawSubscription source "keydown" (pure <<< keyHandler keyName message)

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
