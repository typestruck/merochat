--missing tests
module Server.Recover.Action where

import Prelude
import Server.Effect

import Data.Maybe (Maybe(..))
import Data.UUID as DU
import Run as R
import Server.AccountValidation as SA
import Server.Database.Users (By(..))
import Server.Database.Users as SDU
import Server.Email (Email(..))
import Server.Email as SE
import Server.Recover.Database as SRD
import Server.Response as SR
import Server.Token as ST
import Shared.Account (ResetPassword, EmailCaptcha)

accountNotFound ∷ String
accountNotFound = "Could not find an account with this email"

invalidRecovery ∷ String
invalidRecovery = "Invalid recovery link"

recover ∷ EmailCaptcha → ServerEffect Unit
recover rec = do
      email ← SA.validateEmail rec.email
      record ← SDU.userBy $ Email email
      case record of
            Nothing → SR.throwBadRequest accountNotFound
            Just user → do
                  token ← R.liftEffect (DU.toString <$> DU.genUUID)
                  id ← SRD.insertRecover user.id token
                  SE.sendEmail $ Reset { user_id: user.id, token, email }

reset ∷ ResetPassword → ServerEffect Unit
reset { token, password } = do
      void $ SA.validatePassword password
      maybeId ← SRD.selectRecoverer token
      case maybeId of
            Nothing → SR.throwBadRequest invalidRecovery
            Just id → do
                  hashedPassword ← ST.hashPassword password
                  SRD.recoverPassword token id hashedPassword
