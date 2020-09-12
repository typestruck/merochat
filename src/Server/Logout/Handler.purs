module Server.Logout.Handler where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Payload.Headers as PH
import Payload.ResponseTypes (Response)
import Payload.Server.Response as PSR
import Server.Cookies as SC
import Shared.Routes (routes)

logout :: String -> Effect (Response String)
logout route = pure <<< PSR.setHeaders headers $ PSR.found ""
    where headers = PH.fromFoldable [Tuple "Location" $ routes.login.get { query: {next: Just route }}, SC.makeExpiredCookieHeader ]
