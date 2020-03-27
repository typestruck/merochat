module Client.IM.Scroll where

import Prelude

import Client.Common as CC
import Effect(Effect)

scrollLastMessage :: Effect Unit
scrollLastMessage = CC.scrollDown ".message-history-wrapper"