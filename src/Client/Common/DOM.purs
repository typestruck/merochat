module Client.Common.DOM where

import Prelude

import Control.Monad.Error.Class as CMEC
import Data.Argonaut.Decode as DAD
import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Either (Either(..))
import Data.Either as DE
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.MediaType (MediaType(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception as EE
import Partial.Unsafe as UP
import Type.Data.Boolean (kind Boolean)
import Web.DOM.Document as WDD
import Web.DOM.Element (Element)
import Web.DOM.Element as WDE
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode as WDP
import Web.Event.Event (EventType)
import Web.Event.EventTarget as WET
import Web.Event.Internal.Types (Event)
import Web.HTML as WH
import Web.HTML.HTMLDocument as WHHD
import Web.HTML.HTMLInputElement as WHHI
import Web.HTML.Location as WHL
import Debug.Trace
import Web.HTML.Window as WHW
import Web.Storage.Storage as WSS
import Shared.Header (xAccessToken)

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

