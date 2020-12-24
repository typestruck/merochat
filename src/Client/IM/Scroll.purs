module Client.IM.Scroll where

import Prelude

import Client.Common.DOM as CCD
import Effect (Effect)
import Shared.Unsafe as SU
import Web.DOM.Element as WDE

scrollLastMessage :: Effect Unit
scrollLastMessage = do
      node <- SU.fromJust <<< WDE.fromNode <<< WDE.toNode <$> CCD.unsafeQuerySelector "#message-history"
      CCD.scrollDown node