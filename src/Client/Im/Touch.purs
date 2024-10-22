module Client.Im.Swipe where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Flame.Html.Event as HA
import Flame.Types (NodeData)
import Shared.Im.Types (Touch)
import Web.Event.Internal.Types (Event)

foreign import touchStart ∷ EffectFn1 Event Unit
foreign import touchEnd ∷ EffectFn1 Event (Array Int)

onTouchStart ∷ ∀ message. Maybe message -> NodeData message
onTouchStart message = HA.createRawEvent "touchstart" handler
      where
      handler event = do
            EU.runEffectFn1 touchStart event
            pure message

onTouchEnd ∷ ∀ message. (Touch → message) → NodeData message
onTouchEnd message = HA.createRawEvent "touchend" handler
      where
      handler event = do
            tes ← EU.runEffectFn1 touchEnd event
            pure <<< Just $ message case tes of
                  [ tsx, tex, tsy, tey ] → { startX: tsx, endX: tex, startY: tsy, endY: tey }
                  _ → { startX: -1, endX: -1, startY: -1, endY: -1 }