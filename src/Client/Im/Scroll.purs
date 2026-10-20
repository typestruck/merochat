module Client.Im.Scroll where

import Prelude

import Client.Ui.Document as CUD
import Client.Ui.Scroll as CUS
import Client.Ui.Event as CUEV
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as EC
import Flame.Html.Event (Event)
import Shared.Element (ElementId(..))
import Shared.Element as SE
import Shared.Im.Types (ImMessage)
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.HTML.HTMLElement as WHHE

-- | When jumping into a chat history the last message should be visible
scrollLastMessage ∷ Effect Unit
scrollLastMessage = do
      node ← CUD.unsafeGetElementById MessageHistory
      CUS.scrollDown node

scrollLastMessageAff ∷ Aff (Maybe ImMessage)
scrollLastMessageAff = do
      EC.liftEffect scrollLastMessage
      pure Nothing

scrollIntoView ∷ ∀ r. { id ∷ Int | r } → Effect Unit
scrollIntoView message = do
      element ← CUD.unsafeQuerySelector $ "#" <> SE.messageElementId message.id
      CUS.scrollIntoView element

isScrolledDown ∷ Event → Effect Boolean
isScrolledDown event = do
      let element = SU.fromJust $ CUEV.target event >>= WDE.fromEventTarget
      top ← WDE.scrollTop element
      height ← WDE.scrollHeight element
      offset ← WHHE.offsetHeight <<< SU.fromJust $ WHHE.fromElement element
      pure $ top + lenience >= height - offset

      where
      lenience = 42.0
