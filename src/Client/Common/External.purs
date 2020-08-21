module Client.Common.External where

import Prelude

import Client.Common.DOM as CCD
import Client.Common.Notification as CCN
import Data.Maybe (Maybe(..))
import Data.String as DS
import Effect (Effect)
import Shared.Types (RegisterLogin)

-- | Abstracts the validation common to register and login
validateEmailPassword :: Effect (Maybe RegisterLogin)
validateEmailPassword = do
      emailElement <- CCD.querySelector "#email"
      passwordElement <- CCD.querySelector "#password"
      email <- CCD.value emailElement
      password <- CCD.value passwordElement

      if DS.null email || DS.null password then do
            CCN.alert "Email and password are mandatory"
            pure Nothing
       else pure $ Just {
                email: email,
                password: password,
                captchaResponse: Nothing
           }