module Client.Common.DOM where

import Prelude

import Data.Function.Uncurried (Fn2, Fn1)
import Data.Function.Uncurried as DFU
import Data.Maybe as DM
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Exception as EE
import Effect.Uncurried (EffectFn1, EffectFn2)
import Effect.Uncurried as EU
import Shared.Unsafe as SU
import Web.DOM.Document as WDD
import Web.DOM.Element (Element)
import Web.DOM.Element as WDE
import Web.DOM.Element as WHE
import Web.DOM.Node as WDN
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode as WDP
import Web.Event.CustomEvent (CustomEvent)
import Web.Event.CustomEvent as WEC
import Web.Event.Event (EventType(..))
import Web.Event.Event as WEE
import Web.Event.EventTarget as WET
import Web.Event.Internal.Types (Event)
import Web.HTML as WH
import Web.HTML.HTMLDocument as WHHD
import Web.HTML.HTMLElement as WHHE
import Web.HTML.HTMLInputElement as WHHI
import Web.HTML.HTMLScriptElement as WHS
import Web.HTML.Window as HWH
import Web.HTML.Window as WHW
import Web.UIEvent.KeyboardEvent as WUK
import Web.UIEvent.KeyboardEvent.EventTypes (keyup)

foreign import innerHTML_ :: EffectFn2 Element String Unit
foreign import innerText_ :: EffectFn1 Element String

foreign import createCustomEvent_ :: Fn2 String String CustomEvent
foreign import customEventDetail_ :: Fn1 CustomEvent String

foreign import documentHasFocus :: Effect Boolean

nameChanged :: EventType
nameChanged = EventType "nameChanged"

dispatchCustomEvent :: CustomEvent -> Effect Unit
dispatchCustomEvent event = do
      window <- WH.window
      document <- WHHD.toDocument <$> WHW.document window
      void $ WET.dispatchEvent (WEC.toEvent event) $ WDD.toEventTarget document

createCustomEvent :: EventType -> String -> CustomEvent
createCustomEvent (EventType name) = DFU.runFn2 createCustomEvent_ name

addCustomEventListener :: EventType -> (String -> Effect Unit) -> Effect Unit
addCustomEventListener eventType handler = do
      window <- WH.window
      document <- WHHD.toDocument <$> WHW.document window
      listener <- WET.eventListener (handler <<< DFU.runFn1 customEventDetail_ <<< SU.fromJust <<< WEC.fromEvent)
      WET.addEventListener eventType listener false $ WDD.toEventTarget document

confirm :: String -> Effect Boolean
confirm message = do
      window <- WH.window
      HWH.confirm message window

-- | Adds an event to the given element.
addEventListener :: forall a . Element -> EventType -> (Event -> Effect a) -> Effect Unit
addEventListener element eventType handler = do
      listener <- WET.eventListener handler
      WET.addEventListener eventType listener false $ WDE.toEventTarget element

-- | Selects a single element.
querySelector :: String -> Effect Element
querySelector selector = do
      window <- WH.window
      document <- WHHD.toDocument <$> WHW.document window
      maybeElement <- WDP.querySelector (QuerySelector selector) $ WDD.toParentNode document
      DM.maybe (EE.throwException $ EE.error $ "Selector returned no nodes:" <> selector) pure maybeElement

scrollDown :: Element -> Effect Unit
scrollDown element = do
      height <- WDE.scrollHeight element
      WDE.setScrollTop height element

value :: Element -> Effect String
value element = DM.maybe inputException WHHI.value $ WHHI.fromElement element
      where inputException = do
                  id <- WDE.id element
                  EE.throwException <<< EE.error $ "Element is not an input type" <> id

setInnerHTML :: Element -> String -> Effect Unit
setInnerHTML element = EU.runEffectFn2 innerHTML_ element

innerTextFromTarget :: Event -> Effect String
innerTextFromTarget event = EU.runEffectFn1 innerText_ $ SU.fromJust do
      target <- WEE.target event
      WDE.fromEventTarget target

loadScript :: String -> Effect Unit
loadScript name = do
      window <- WH.window
      document <- WHW.document window
      script <- WDD.createElement "script" $ WHHD.toDocument document
      WHS.setSrc ("/client/javascript/"<>name) <<< SU.fromJust $ WHS.fromElement script
      body <- SU.fromJust <$> WHHD.body document
      void <<< WDN.appendChild (WHE.toNode script) $ WHHE.toNode body

onEnter :: Element -> Effect Unit -> Effect Unit
onEnter element action = do
      addEventListener element keyup go
      where go event = do
                  let pressed = WUK.key <<< SU.fromJust $ WUK.fromEvent event
                  when (pressed == "Enter") action

preventStop :: Event -> Effect Unit
preventStop event = do
      WEE.preventDefault event
      WEE.stopPropagation event