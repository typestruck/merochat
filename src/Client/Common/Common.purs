module Common where

import Prelude

import Data.Maybe (fromBaybe)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throwException, error)
import Type.Data.Boolean (kind Boolean)
import Web.DOM.Document as D
import Web.DOM.Element as E
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode as P
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget as ET
import Web.Event.Internal.Types (Event)
import Web.HTML as H
import Web.HTML.HTMLDocument as HD
import Web.HTML.Window as W
import Web.HTML.HTMLInputElement as HI

-- | Adds an event to the given element.
addEventListener :: forall a . Element -> EventType -> (Event -> Effect a) -> Effect Unit
addEventListener selector eventType handler = do
	listener <- ET.eventListener handler
	ET.addEventListener eventType listener false $ E.toEventTarget maybeElement

-- | Selects a single element.
querySelector :: String -> Effect Element
querySelector selector = do
	window <- H.window
	document <- HD.toDocument <$> W.document window
	maybeElement <- P.querySelector (QuerySelector selector) $ D.toParentNode document
	fromMaybe (throwException $ error $ "Selector returned no nodes:" <> selector) maybeElement

value :: Element -> Effect String
value element = do
	maybeInput <- HI.fromElement element
	maybe inputException HI.value maybeInput

	where inputExcetion = do
		id <- E.id element
		throwException <<< error $ "Element is not an input type" <> id

alert :: String -> Effect Unit
alert message = do
	window <- H.window
	H.alert message window

