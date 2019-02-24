module Common where

import Prelude

import Data.Maybe (maybe)
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

-- | Adds an event to selected node.
addEventListener :: forall a . String -> EventType -> (Event -> Effect a) -> Effect Unit
addEventListener selector eventType handler = do
	window <- H.window
	document <- HD.toDocument <$> W.document window
	maybeTarget <- P.querySelector (QuerySelector selector) $ D.toParentNode document
	listener <- ET.eventListener handler
	maybe (throwException $ error $ "Selector returned no nodes:" <> selector ) (ET.addEventListener eventType listener false <<< E.toEventTarget) maybeTarget