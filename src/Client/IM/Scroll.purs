module Client.IM.Scroll where

import Prelude

import Client.Common.DOM as CCD
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Shared.Types (IMMessage)

scrollLastMessage :: Effect Unit
scrollLastMessage = do
      node <- CCD.unsafeQuerySelector "#message-history"
      CCD.scrollDown node

scrollLastMessage' :: Aff (Maybe IMMessage)
scrollLastMessage' = do
      liftEffect scrollLastMessage
      pure Nothing