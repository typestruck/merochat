module Server.Logout where

import Prelude

import Data.Tuple (Tuple(..))
import Payload.Headers as PH
import Payload.ResponseTypes (Response)
import Payload.Server.Response as PSR
import Server.Cookies as SC
import Server.Ok (ok)
import Server.Types (Ok)

logout :: forall r. String -> r -> Response r
logout route = PSR.setHeaders headers <<< PSR.found
    where headers = PH.fromFoldable [Tuple "Location" route, SC.makeExpiredCookieHeader]

expireCookies :: Response Ok
expireCookies = PSR.setHeaders headers $ PSR.ok ok
    where headers = PH.fromFoldable [SC.makeExpiredCookieHeader]
