module Shared.Types where

import Prelude
import Data.Maybe (Maybe)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show as S

-- | Fields for registration or login
newtype RegisterLogin = RegisterLogin
	{
	        email:: String,
	        password:: String,
	        captchaResponse:: Maybe String
	}

derive instance genericRegisterLogin :: Generic RegisterLogin _

-- | tokenPOST is a mitigation for csrf/cookie interception (since httpure http doesn't seem to offer any sort of antiforgery tokens) used for post requests, whereas tokenGET is used for (login restricted) get requests, since I don't to make it a single page application
newtype Token = Token { tokenGET :: String, tokenPOST :: String}

derive instance genericToken :: Generic Token _

-- | All available endpoints for melanchat
data Route = Landing | Register | Login { next :: Maybe String } | IM

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route

instance showMyRecord :: Show Route where
	show = S.genericShow

newtype BadRequest = BadRequest {reason :: String}

derive instance genericBadRequest :: Generic BadRequest _


