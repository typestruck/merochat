module Client.Im.Scroll where

import Prelude

import Client.Dom as CCD
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Shared.Element (ElementId(..))
import Shared.Im.Types (HistoryMessage, ImMessage)
import Shared.Unsafe as SU
import Web.DOM.Element (Element)
import Web.DOM.Element as WDE
import Web.Event.Event (target)
import Web.Event.Event as WEE
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLElement as WHH

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

isScrolledDown ∷ Event → Effect Boolean
isScrolledDown event = do
      let element = SU.fromJust (WEE.target event >>= WDE.fromEventTarget)
      top ← WDE.scrollTop element
      height ← WDE.scrollHeight element
      offset ← WHH.offsetHeight <<< SU.fromJust $ WHH.fromElement element
      pure $ top + lenience >= height - offset

      where
      lenience = 42.0