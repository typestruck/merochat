--missing tests
module Server.Recover.Action where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe (Maybe(..))
import Data.UUID as DU
import Run as R
import Server.AccountValidation as SA
import Server.Captcha as SC
import Server.Database.User as SDU
import Server.Email as SE
import Server.Ok (ok)
import Server.Recover.Database as SRD
import Server.Response as SR
import Server.Token as ST

accountNotFound :: String
accountNotFound = "Could not find an account with this email"

invalidRecovery :: String
invalidRecovery = "Invalid recovery link"

recover :: RecoverAccount -> ServerEffect Ok
recover { email: rawEmail, captchaResponse } = do
      SC.validateCaptcha captchaResponse
      email <- SA.validateEmail rawEmail
      user <- SDU.userBy $ Email email
      case user of
            Nothing -> SR.throwBadRequest accountNotFound
            Just (RegisterLoginUser { id }) -> do
                  token <- R.liftEffect (DU.toString <$> DU.genUUID)
                  SRD.insertRecover id token
                  --REFACTOR: use shared.options.domain and shared.routes
                  SE.sendEmail email $ """Hello! <br> <a href="https://melan.chat/recover?token=""" <> token <> """">Click here to reset your password</a>. If you didn't ask to recover your password, just ignore this email. Your account will be safe."""
                  pure ok

reset :: ResetPassword -> ServerEffect Ok
reset { token, password } = do
      hash <- SA.validatePassword password
      maybeID <- SRD.selectRecoverer token
      case maybeID of
            Nothing -> SR.throwBadRequest invalidRecovery
            Just id -> do
                  hashedPassword <- ST.hashPassword password
                  SRD.recoverPassword token id hashedPassword
                  pure ok
