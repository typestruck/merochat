module Shared.Types where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, (.:), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, (~>), (:=))
import Data.Argonaut.Core(jsonEmptyObject)
import Data.Maybe (Maybe)

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