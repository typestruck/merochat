module Client.Common.Location where

import Debug.Trace
import Prelude

import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.MediaType (MediaType(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception as EE
import Partial.Unsafe as UP
import Shared.Header (xAccessToken)
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
import Web.HTML.Window as WHW
import Web.Storage.Storage as WSS

--REFACTOR: there urls must be Route
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

path :: Effect String
path = do
        window <- WH.window
        location <- WHW.location window
        search <-WHL.search location
        pathname <- WHL.pathname location
        pure $ pathname <> search
