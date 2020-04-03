module Client.Common.Location where

import Prelude

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

setLocation :: String -> Effect Unit
setLocation url = do
        window <- WH.window
        location <- WHW.location window
        WHL.setHref url location

search :: Effect String
search = do
        window <- WH.window
        location <- WHW.location window
        WHL.search location