module Shared.ResizeInput where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as EC
import Shared.Unsafe as SU
import Web.DOM (Element)
import Web.DOM.Element as WDE
import Web.Event.Event as WEE
import Web.Event.Internal.Types (Event)

foreign import resizeInput ∷ Element → Effect Unit

-- | The chatting textarea grows / shrink as text is inputed
resizeInputFrom ∷ ∀ model message. Event → model → model /\ (Array (Aff (Maybe message)))
resizeInputFrom event model = model /\ [ resize ]
      where
      resize = do
            EC.liftEffect <<< resizeInput <<< SU.fromJust $ do
                  target ← WEE.target event
                  WDE.fromEventTarget target
            pure Nothing