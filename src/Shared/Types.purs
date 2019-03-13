module Shared.Types where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, (.:), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, (~>), (:=))
import Data.Argonaut.Core(jsonEmptyObject)
import Data.Maybe (Maybe)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show as S

-- look into generic to aleviate the json boilerplate

newtype RegisterLogin = RegisterLogin
	{
	        email:: String,
	        password:: String,
	        captchaResponse:: Maybe String
	}

instance decodeJsonRegisterLogin :: DecodeJson RegisterLogin where
	decodeJson json = do
		rl <- decodeJson json
		email <- rl .: "email"
		password <- rl .: "password"
		captchaResponse <- rl .: "captchaResponse"
		pure $ RegisterLogin { email, password, captchaResponse }

instance encodeJsonRegisterLogin :: EncodeJson RegisterLogin where
	encodeJson (RegisterLogin rl) =
    		"email" := rl.email
      		~> "password" := rl.password
		~> "captchaResponse" := rl.captchaResponse
      		~> jsonEmptyObject

-- | tokenPOST is a mitigation for csrf/cookie interception (since zinc http doesn't seem to offer any sort of antiforgery tokens) used for post requests, whereas tokenGET is used for (login restricted) get requests, since I don't to make it a single page application
newtype Token = Token { tokenGET :: String, tokenPOST :: String}

instance decodeJsonToken :: DecodeJson Token where
	decodeJson json = do
		t <- decodeJson json
		tokenGET <- t .: "tokenGET"
		tokenPOST <- t .: "tokenGET"
		pure $ Token { tokenGET, tokenPOST }

data Route = Landing | Register | Login { next :: Maybe String } | IM

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route

instance showMyRecord :: Show Route where
	show = S.genericShow

newtype BadRequest = BadRequest {reason :: String}

derive instance genericRoute :: Generic BadRequest _


