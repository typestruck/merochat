module Client.Common.Location where

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