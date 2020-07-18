module Server.Recover.Action where

import Prelude
import Server.Types
import Shared.Types

import Data.Either (Either(..))
import Data.Either as DE
import Data.Maybe as DM
import Data.String as DS
import Run as R
import Server.Database.User as SDU
import Server.Captcha as SC
import Data.UUID as DU
import Run.Reader as RR
import Server.Response as SRR

invalidEmailMessage :: String
invalidEmailMessage = "Invalid email"

accountNotFound :: String
accountNotFound = "Could not find an account with this email"

recover :: RecoverPassword -> ServerEffect Ok
recover (RecoverPassword { email, captchaResponse }) = do
      when (DS.null email) $ SRR.throwBadRequest invalidEmailMessage
      user <- SDU.userBy $ Email email
      when (DM.isNothing user) $ SRR.throwBadRequest accountNotFound
      SC.validateCaptcha captchaResponse

      token <- R.liftEffect (DU.toString <$> DU.genUUID)
      SRD.insertRecover { email, token }

      pure Ok

