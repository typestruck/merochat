module Server.Recover.Action where

import Prelude
import Server.Types
import Shared.Types

import Data.Either (Either(..))
import Data.Either as DE
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.UUID as DU
import Run as R
import Run.Reader as RR
import Server.Captcha as SC
import Server.Database.User as SDU
import Server.Email as SE
import Server.Ok (ok)
import Server.Recover.Database as SRD
import Server.Response as SR

import Server.Token as ST

invalidEmailMessage :: String
invalidEmailMessage = "Invalid email"

blankPasswordMessage :: String
blankPasswordMessage = "Password is required"

accountNotFound :: String
accountNotFound = "Could not find an account with this email"

invalidRecover :: String
invalidRecover = "Invalid recovery link"

recover :: RecoverAccount -> ServerEffect Ok
recover { email, captchaResponse } = do
      when (DS.null email) $ SR.throwBadRequest invalidEmailMessage
      user <- SDU.userBy $ Email email
      case user of
            Nothing -> SR.throwBadRequest accountNotFound
            Just (RegisterLoginUser { id }) -> do
                  SC.validateCaptcha captchaResponse
                  token <- R.liftEffect (DU.toString <$> DU.genUUID)
                  SRD.insertRecover id token
                  SE.sendEmail email $ """Hello! <br> <a href="https://melan.chat/recover?token=""" <> token <> """">Click here to reset your password</a>. If you didn't ask to recover your password, just ignore this email. Your account will be safe."""
                  pure ok

reset :: ResetPassword -> ServerEffect Ok
reset { token, password } = do
      when (DS.null password) $ SR.throwBadRequest blankPasswordMessage
      maybeID <- SRD.selectRecoverer token
      case maybeID of
            Nothing -> SR.throwBadRequest invalidRecover
            Just id -> do
                  hashedPassword <- ST.hashPassword password
                  SRD.recoverPassword token id hashedPassword
                  pure ok
