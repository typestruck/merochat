module Server.Logout where

import Prelude

import Data.Tuple (Tuple(..))
import Payload.Headers as PH
import Payload.ResponseTypes (Empty(..), Response)
import Payload.Server.Response as PSR
import Server.Cookies as SC

logout ∷ ∀ r. String → r → Response r
logout route = PSR.setHeaders headers <<< PSR.found
      where
      headers = PH.fromFoldable [ Tuple "Location" route, SC.makeExpiredCookieHeader ]

expireCookies ∷ Response Empty
expireCookies = PSR.setHeaders headers $ PSR.ok Empty
      where
      headers = PH.fromFoldable [ SC.makeExpiredCookieHeader ]
