module Client.IM.Scroll where

import Prelude

import Client.Common.DOM as CCD
import Effect (Effect)

scrollLastMessage :: Effect Unit
scrollLastMessage = do
      node <- CCD.unsafeQuerySelector "#message-history"
      CCD.scrollDown node