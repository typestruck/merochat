module Client.Ui.Event where

import Prelude

import Data.Function.Uncurried (Fn1, Fn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as DN
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Foreign (Foreign)
import Shared.Unsafe as SU
import Web.DOM.Element (Element)
import Web.DOM.Element as WDE
import Web.Event.CustomEvent (CustomEvent)
import Web.Event.Event (EventType)
import Web.Event.EventTarget (EventTarget)
import Web.Event.EventTarget as WEET
import Web.Event.Internal.Types (Event)
import Web.File.FileList (FileList)

foreign import innerText_ ∷ EffectFn1 Element String

foreign import createCustomEvent_ ∷ ∀ value. Fn2 String value CustomEvent

foreign import customEventDetail_ ∷ ∀ value. Fn1 CustomEvent value

foreign import target_ ∷ Foreign → Nullable EventTarget

foreign import preventDefault ∷ Foreign → Effect Unit

foreign import stopPropagation ∷ Foreign → Effect Unit

foreign import key ∷ Foreign → String

foreign import shiftKey ∷ Foreign → Boolean

foreign import files_ ∷ Foreign → Nullable FileList

-- | Adds an event to the given element
addEventListener ∷ ∀ a. Element → EventType → (Event → Effect a) → Effect Unit
addEventListener element eventType handler = do
      listener ← WEET.eventListener handler
      WEET.addEventListener eventType listener false $ WDE.toEventTarget element

innerTextFromTarget ∷ Foreign → Effect String
innerTextFromTarget event = EU.runEffectFn1 innerText_ $ SU.fromJust do
      eventTarget ← target event
      WDE.fromEventTarget eventTarget

tagNameFromTarget ∷ Foreign → String
tagNameFromTarget event = WDE.tagName $ SU.fromJust do
      eventTarget ← target event
      WDE.fromEventTarget eventTarget

-- | Native payloads do not contain browser DOM targets.
target ∷ Foreign → Maybe EventTarget
target = DN.toMaybe <<< target_

files ∷ Foreign → Maybe FileList
files = DN.toMaybe <<< files_

preventStop ∷ Foreign → Effect Unit
preventStop event = preventDefault event *> stopPropagation event
