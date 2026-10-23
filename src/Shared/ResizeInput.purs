module Shared.ResizeInput where

import Prelude

import Client.Ui.Event as CUEV
import Data.Maybe (Maybe(..))
import Data.Foldable as DF
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as EC
import Flame.Html.Event (Event)
import Web.DOM (Element)
import Web.DOM.Element as WDE

foreign import resizeInput ∷ Element → Effect Unit

-- | The chatting textarea grows / shrink as text is inputed
resizeInputFrom ∷ ∀ model message. Event → model → model /\ (Array (Aff (Maybe message)))
resizeInputFrom event model = model /\ [ resize ]
      where
      resize = do
            EC.liftEffect <<< DF.traverse_ resizeInput $ do
                  target ← CUEV.target event
                  WDE.fromEventTarget target
            pure Nothing
