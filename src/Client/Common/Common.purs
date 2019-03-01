module Common where

import Prelude

import Data.Maybe as M
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throwException, error)
import Type.Data.Boolean (kind Boolean)
import Web.DOM.Document as D
import Web.DOM.Element as E
import Affjax as AJ
import Web.DOM.Element(Element)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode as P
import Web.Event.Event (EventType(..))
import Affjax.ResponseFormat as RF
import Affjax.RequestBody as RB
import Web.Event.EventTarget as ET
import Web.Event.Internal.Types (Event)
import Web.HTML as H
import Web.HTML.HTMLDocument as HD
import Effect.Aff(Aff)
import Data.Either(Either(..))
import Web.HTML.Window as W
import Web.HTML.HTMLInputElement as HI
import Data.Either as DET
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Control.Monad.Error.Class(throwError)

-- | Adds an event to the given element.
addEventListener :: forall a . Element -> EventType -> (Event -> Effect a) -> Effect Unit
addEventListener element eventType handler = do
	listener <- ET.eventListener handler
	ET.addEventListener eventType listener false $ E.toEventTarget element

-- | Selects a single element.
querySelector :: String -> Effect Element
querySelector selector = do
	window <- H.window
	document <- HD.toDocument <$> W.document window
	maybeElement <- P.querySelector (QuerySelector selector) $ D.toParentNode document
	M.maybe (throwException $ error $ "Selector returned no nodes:" <> selector) pure maybeElement

value :: Element -> Effect String
value element = M.maybe inputException HI.value $ HI.fromElement element
	where inputException = do
		id <- E.id element
		throwException <<< error $ "Element is not an input type" <> id

alert :: String -> Effect Unit
alert message = do
	window <- H.window
	W.alert message window

post :: forall a b . EncodeJson a => DecodeJson b => String -> a -> Aff b
post url data' = do
	response <- AJ.post RF.json url <<< RB.json $ encodeJson data'
	DET.either (parseError <<< AJ.printResponseFormatError) (DET.either parseError pure <<< decodeJson)response.body
	where parseError = throwError <<< error <<< ("Could not parse json: " <> _)
