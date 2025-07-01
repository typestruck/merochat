--this can be broken down into modules
module Client.Common.Dom where

import Prelude
import Shared.Im.Types

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn1)
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Data.Nullable (Nullable)
import Data.Nullable as DN
import Effect (Effect)
import Effect.Exception as EE
import Effect.Uncurried (EffectFn1, EffectFn2)
import Effect.Uncurried as EU
import Shared.Element (ElementId)
import Shared.Resource (Bundle, ResourceType(..))
import Shared.Resource as SP
import Shared.Unsafe as SU
import Type.Row.Homogeneous (class Homogeneous)
import Web.DOM.Document as WDD
import Web.DOM.Element (Element)
import Web.DOM.Element as WDE
import Web.DOM.Element as WHE
import Web.DOM.Node as WDN
import Web.DOM.NonElementParentNode as WDNE
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode as WDP
import Web.Event.CustomEvent (CustomEvent)
import Web.Event.Event (EventType(..))
import Web.Event.Event as WEE
import Web.Event.EventTarget as WET
import Web.Event.Internal.Types (Event)
import Web.HTML (Navigator, Window)
import Web.HTML as WH
import Web.HTML.HTMLDocument as WHHD
import Web.HTML.HTMLElement as WHHE
import Web.HTML.HTMLScriptElement as WHS
import Web.HTML.Window as HWH
import Web.HTML.Window as WHW
import Web.UIEvent.KeyboardEvent as WUK
import Web.UIEvent.KeyboardEvent.EventTypes (keyup)

foreign import innerHTML_ ∷ EffectFn2 Element String Unit
foreign import removeFromClassList_ ∷ EffectFn2 Element String Unit
foreign import addToClassList_ ∷ EffectFn2 Element String Unit
foreign import innerText_ ∷ EffectFn1 Element String
foreign import pushState_ ∷ EffectFn1 String Unit

foreign import isMediaTypeSupported_ ∷ EffectFn1 String Boolean

foreign import createCustomEvent_ ∷ ∀ value. Fn2 String value CustomEvent
foreign import customEventDetail_ ∷ ∀ value. Fn1 CustomEvent value

foreign import value_ ∷ EffectFn1 Element String
foreign import setValue_ ∷ EffectFn2 Element String Unit

foreign import toggleDisabled_ ∷ EffectFn1 Element Unit

foreign import documentHasFocus ∷ Effect Boolean

foreign import documentIsNotHidden ∷ Effect Boolean

foreign import screenWidth ∷ Effect Int

foreign import requestNotificationPermission ∷ Effect Unit
foreign import notificationPermission ∷ Effect String

foreign import scrollIntoView_ ∷ EffectFn1 Element Unit

foreign import mediaMatches_ ∷ EffectFn1 String Boolean

mediaMatches ∷ String → Effect Boolean
mediaMatches = EU.runEffectFn1 mediaMatches_

setChatExperiment ∷ EventType
setChatExperiment = EventType "setChatExperiment"

confirm ∷ String → Effect Boolean
confirm message = do
      window ← WH.window
      HWH.confirm message window

-- | Adds an event to the given element
addEventListener ∷ ∀ a. Element → EventType → (Event → Effect a) → Effect Unit
addEventListener element eventType handler = do
      listener ← WET.eventListener handler
      WET.addEventListener eventType listener false $ WDE.toEventTarget element

-- | Selects a single element
unsafeGetElementById ∷ ElementId → Effect Element
unsafeGetElementById elementId = do
      maybeElement ← getElementById elementId
      DM.maybe (EE.throwException $ EE.error $ "No element with id:" <> show elementId) pure maybeElement

getElementById ∷ ElementId → Effect (Maybe Element)
getElementById elementId = do
      window ← WH.window
      document ← WHHD.toDocument <$> WHW.document window
      WDNE.getElementById (show elementId) $ WDD.toNonElementParentNode document

-- | Selects a single element.
unsafeQuerySelector ∷ String → Effect Element
unsafeQuerySelector selector = do
      maybeElement ← querySelector selector
      DM.maybe (EE.throwException $ EE.error $ "Selector returned no nodes:" <> selector) pure maybeElement

querySelector ∷ String → Effect (Maybe Element)
querySelector selector = do
      window ← WH.window
      document ← WHHD.toDocument <$> WHW.document window
      WDP.querySelector (QuerySelector selector) $ WDD.toParentNode document

scrollDown ∷ Element → Effect Unit
scrollDown element = do
      height ← WDE.scrollHeight element
      WDE.setScrollTop height element

scrollIntoView ∷ Element → Effect Unit
scrollIntoView element = EU.runEffectFn1 scrollIntoView_ element

-- | Field value property or button inner text
value ∷ Element → Effect String
value element = EU.runEffectFn1 value_ element

setValue ∷ Element → String → Effect Unit
setValue element value = EU.runEffectFn2 setValue_ element value

toggleDisabled ∷ Element → Effect Unit
toggleDisabled = EU.runEffectFn1 toggleDisabled_

setInnerHTML ∷ Element → String → Effect Unit
setInnerHTML element = EU.runEffectFn2 innerHTML_ element

innerTextFromTarget ∷ Event → Effect String
innerTextFromTarget event = EU.runEffectFn1 innerText_ $ SU.fromJust do
      target ← WEE.target event
      WDE.fromEventTarget target

tagNameFromTarget ∷ Event → String
tagNameFromTarget event = WDE.tagName $ SU.fromJust do
      target ← WEE.target event
      WDE.fromEventTarget target

createElement ∷ String → Effect Element
createElement tag = do
      window ← WH.window
      document ← WHW.document window
      WDD.createElement tag $ WHHD.toDocument document

preventStop ∷ Event → Effect Unit
preventStop event = do
      WEE.preventDefault event
      WEE.stopPropagation event

setTitle ∷ String → Effect Unit
setTitle title = do
      window ← WH.window
      document ← WHW.document window
      WHHD.setTitle title document

pushState ∷ String → Effect Unit
pushState = EU.runEffectFn1 pushState_

removeFromClassList ∷ Element → String → Effect Unit
removeFromClassList = EU.runEffectFn2 removeFromClassList_

addToClassList ∷ Element → String → Effect Unit
addToClassList = EU.runEffectFn2 addToClassList_

acceptedAudioCodec ∷ Effect String
acceptedAudioCodec = do
      isIt ← EU.runEffectFn1 isMediaTypeSupported_ appleShit
      if isIt then pure appleShit else pure webm
      where
      appleShit = "video/mp4;codecs=avc1"
      webm = "audio/webm"
