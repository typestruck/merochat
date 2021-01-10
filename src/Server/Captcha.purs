module Server.Captcha where

import Prelude
import Server.Types

import Affjax as A
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Decode as DAD
import Data.Either (Either(..))
import Data.Either as DE
import Data.FormURLEncoded as DF
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Environment (development)
import Run as R
import Run.Reader as RR
import Server.Response as SR

validateCaptcha :: Maybe String -> ServerEffect Unit
validateCaptcha captchaResponse = do
      { configuration : { captchaSecret } } <- RR.ask
      unless development do
            response <- R.liftAff <<< A.request $ A.defaultRequest {
                  url = "https://www.google.com/recaptcha/api/siteverify",
                  method = Left POST,
                  responseFormat = RF.json,
                  content = Just <<< RB.formURLEncoded $ DF.fromArray [
                        Tuple "secret" $ Just captchaSecret,
                        Tuple "response" captchaResponse
                  ]
            }
            case response of
                  Right { status, body, statusText } ->
                        if status == StatusCode 200 then
                              DE.either (SR.throwInternalError <<< show) finishWithCaptcha $ DAD.decodeJson body
                        else
                              SR.throwBadRequest statusText
                  Left left -> SR.throwInternalError $ A.printError left

      where finishWithCaptcha (CaptchaResponse { success })
                  | success = pure unit
                  | otherwise = SR.throwBadRequest "Incorrect captcha"
