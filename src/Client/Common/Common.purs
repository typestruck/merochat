module Client.Common(
	setItem,
	post,
	post',
	addEventListener,
	querySelector,
	value,
	setLocation,
	alert,
	tokenKey,
	search
) where

import Prelude

import Affjax as AJ
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.ResponseFormat as RF
import Control.Monad.Error.Class as EC
import Data.Either (Either(..))
import Data.Either as DET
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as M
import Data.MediaType (MediaType(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception as EE
import Type.Data.Boolean (kind Boolean)
import Web.DOM.Document as D
import Web.DOM.Element (Element)
import Web.DOM.Element as E
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode as P
import Web.Event.Event (EventType)
import Web.Event.EventTarget as ET
import Web.Event.Internal.Types (Event)
import Web.HTML as H
import Web.HTML.HTMLDocument as HD
import Web.HTML.HTMLInputElement as HI
import Web.HTML.Location as L
import Web.HTML.Window as W
import Web.Storage.Storage as S
import Data.Argonaut.Encode.Generic.Rep as EGR
import Data.Argonaut.Encode.Generic.Rep(class EncodeRep)
import Data.Argonaut.Decode.Generic.Rep as DGR
import Data.Argonaut.Decode.Generic.Rep(class DecodeRep)
import Data.Generic.Rep(class Generic)

tokenKey :: String
tokenKey = "token"

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
	M.maybe (EE.throwException $ EE.error $ "Selector returned no nodes:" <> selector) pure maybeElement

value :: Element -> Effect String
value element = M.maybe inputException HI.value $ HI.fromElement element
	where inputException = do
		id <- E.id element
		EE.throwException <<< EE.error $ "Element is not an input type" <> id

alert :: String -> Effect Unit
alert message = do
	window <- H.window
	W.alert message window

-- | A simplified version of post without the option to handle errors
post' :: forall a c d e f. Generic d c => EncodeRep c => Generic f e => DecodeRep e => String -> d -> (f -> Aff a) -> Aff Unit
post' url data' success = post url data' success (parseJSONError <<< AJ.printResponseFormatError)

-- | Performs a POST request
post :: forall a b c d e f. Generic d c => EncodeRep c => Generic f e => DecodeRep e => String -> d -> (f -> Aff a) -> (ResponseFormatError -> Aff b) -> Aff Unit
post url data' success error = do
	--see Token in shared/Types.purs
	token <- liftEffect $ getItem tokenKey
	request url POST [RequestHeader "x-access-token" token] data' success error

-- using callbacks avoids here actually simplifies things as 99% of requests will be the same parsing/error handling
-- | Performs a HTTP request sending JSON
request :: forall a b c d e f. Generic d c => EncodeRep c => Generic f e => DecodeRep e => String -> Method -> Array RequestHeader -> d -> (f -> Aff a) -> (ResponseFormatError -> Aff b) -> Aff Unit
request url method extraHeaders data' success error = do
	response <- AJ.request $ AJ.defaultRequest {
		url = url,
		method = Left method,
		responseFormat = RF.json,
		headers = [
			Accept $ MediaType "application/json",
			ContentType $ MediaType "application/json"
		] <> extraHeaders,
		content = Just <<< RB.json $ EGR.genericEncodeJson data'
	}
	DET.either error' (DET.either parseJSONError success' <<< DGR.genericDecodeJson) response.body
	pure unit
	where   error' formatError = do
			_ <- error formatError
			pure unit
		success' json = do
			_ <- success json
			pure unit

parseJSONError message = do
	liftEffect $ alert message
	EC.throwError <<< EE.error $ "Could not parse json: " <> message

setLocation :: String -> Effect Unit
setLocation url = do
	window <- H.window
	location <- W.location window
	L.setHref url location

setItem :: String -> String -> Effect Unit
setItem key itemValue = do
	window <- H.window
	localStorage <- W.localStorage window
	S.setItem key itemValue localStorage

getItem :: String  -> Effect String
getItem key = do
	window <- H.window
	localStorage <- W.localStorage window
	M.fromMaybe "" <$> S.getItem key localStorage

search :: Effect String
search = do
	window <- H.window
	location <- W.location window
	L.search location
