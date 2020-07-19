module Server.Captcha where

import Server.Types
import Prelude
import Server.Response as SRR
import Run.Reader as RR
import Run as R
import Data.Either (Either(..))
import Data.Either as DE
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Affjax as A
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Data.FormURLEncoded as DF
import Data.HTTP.Method (Method(..))
import Data.Argonaut.Decode as DAD

validateCaptcha :: Maybe String -> ServerEffect Unit
validateCaptcha captchaResponse = do
      { configuration : Configuration configuration } <- RR.ask

      unless configuration.development do
            response <- R.liftAff <<< A.request $ A.defaultRequest {
                  url = "https://www.google.com/recaptcha/api/siteverify",
                  method = Left POST,
                  responseFormat = RF.json,
                  content = Just <<< RB.formURLEncoded $ DF.fromArray [
                        Tuple "secret" $ Just configuration.captchaSecret,
                        Tuple "response" captchaResponse
                  ]
            }
            case response.body of
                  Right payload ->
                        if response.status == StatusCode 200 then
                              DE.either SRR.throwInternalError finishWithCaptcha $ DAD.decodeJson payload
                        else
                              SRR.throwBadRequest response.statusText
                  Left left -> SRR.throwInternalError $ RF.printResponseFormatError left

      where finishWithCaptcha (CaptchaResponse { success })
                  | success = pure unit
                  | otherwise = SRR.throwBadRequest "Incorrect captcha"
