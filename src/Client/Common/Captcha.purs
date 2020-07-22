module Client.Common.Captcha where

import Prelude
import Effect (Effect)

foreign import grecaptchaExecute :: Effect Unit
foreign import grecaptchaReset :: Effect Unit
