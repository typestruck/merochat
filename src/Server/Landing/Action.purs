module Server.Landing.Action where

import Prelude
import Server.Types
import Shared.Types

import Affjax as A
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Decode as DAD
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Either (Either(..))
import Data.Either as DE
import Data.FormURLEncoded as DF
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Server.Token as ST
import Data.Int53 (Int53)
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Tuple (Tuple(..))
import Effect.Console as EC
import HTTPure (ResponseM)
import HTTPure as H
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Node.HTTP.Client as NTC
import Server.Bender as SB
import Server.Landing.Database as SLD
import Server.Database.User as SDU
import Server.Response as SRR

invalidUserEmailMessage :: String
invalidUserEmailMessage = "Invalid email or password"

emailAlreadyRegisteredMessage :: String
emailAlreadyRegisteredMessage = "Email already registered"

register :: String -> RegisterLogin -> ServerEffect Token
register remoteIP (RegisterLogin registerLogin) = do
        when (DS.null registerLogin.email || DS.null registerLogin.password) $ SRR.throwBadRequest invalidUserEmailMessage

        user <- SDU.userBy $ Email registerLogin.email

        when (DM.isJust user) $ SRR.throwBadRequest emailAlreadyRegisteredMessage

        { configuration : Configuration configuration } <- RR.ask

        if not configuration.development then do
                response <- R.liftAff <<< A.request $ A.defaultRequest {
                                        url = "https://www.google.com/recaptcha/api/siteverify",
                                        method = Left POST,
                                        responseFormat = RF.json,
                                        content = Just <<< RB.formURLEncoded $ DF.fromArray [
                                                Tuple "secret" $ Just configuration.captchaSecret,
                                                Tuple "response" registerLogin.captchaResponse
                                        ]
                                }
                case response.body of
                        Right payload ->
                                if response.status == StatusCode 200 then
                                        DE.either SRR.throwInternalError finishWithCaptcha $ DAD.decodeJson payload
                                else
                                        SRR.throwBadRequest response.statusText
                        Left left -> SRR.throwInternalError $ RF.printResponseFormatError left
         else
                 finish
        where
                finish = do
                        name <- SB.generateName
                        headline <- SB.generateHeadline
                        description <- SB.generateDescription
                        password <- ST.hashPassword registerLogin.password
                        PrimaryKey id <- SLD.createUser {
                                email: registerLogin.email,
                                name,
                                password,
                                headline,
                                description
                        }
                        ST.createToken id

                finishWithCaptcha (CaptchaResponse {success})
                        | success = finish
                        | otherwise = SRR.throwBadRequest "Incorrect captcha"
