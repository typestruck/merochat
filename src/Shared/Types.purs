module Shared.Types where

import Data.Maybe(Maybe)

type RegisterLogin =
	{
	        email:: String,
	        password:: String,
	        captchaResponse:: Maybe String
	}