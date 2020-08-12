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
import Effect.Exception as EE
import Run as R
import Run.Reader as RR
import Server.Response as SRR

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
            case response of
                  Right { status, body, statusText } ->
                        if status == StatusCode 200 then
                              DE.either (SRR.throwInternalError <<< show) finishWithCaptcha $ DAD.decodeJson body
                        else
                              SRR.throwBadRequest statusText
                  Left left -> SRR.throwInternalError $ A.printError left

      where finishWithCaptcha (CaptchaResponse { success })
                  | success = pure unit
                  | otherwise = SRR.throwBadRequest "Incorrect captcha"
