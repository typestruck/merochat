module Client.Im.Scroll where

import Prelude
import Shared.Im.Types (HistoryMessage, ImMessage)

import Client.Common.Dom as CCD
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Shared.Element (ElementId(..))

-- | When jumping into a chat history the last message should be visible
scrollLastMessage ∷ Effect Unit
scrollLastMessage = do
      node ← CCD.unsafeGetElementById MessageHistory
      CCD.scrollDown node

scrollLastMessageAff ∷ Aff (Maybe ImMessage)
scrollLastMessageAff = do
      liftEffect scrollLastMessage
      pure Nothing

scrollIntoView ∷ HistoryMessage → Effect Unit
scrollIntoView message = do
      element ← CCD.unsafeQuerySelector ("#m" <> show message.id)
      CCD.scrollIntoView element