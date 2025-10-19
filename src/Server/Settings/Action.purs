module Server.Settings.Action where

import Prelude
import Server.Effect

import Data.Either (Either(..))
import Data.String as DS
import Droplet.Driver (Pool)
import Run.Except as RE
import Server.AccountValidation as SA
import Server.File as SF
import Server.Sanitize as SS
import Server.Settings.Database as SSD
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.ResponseError (ResponseError(..))
import Shared.Settings.Types (PrivacySettings)

settings ∷ Int → ServerEffect PrivacySettings
settings loggedUserId = SSD.privacySettings loggedUserId

changeEmail ∷ Int → String → ServerEffect Unit
changeEmail loggedUserId rawEmail = do
      email ← SA.validateEmail rawEmail
      SA.validateExistingEmail email
      SSD.changeEmail loggedUserId email

changePassword ∷ Int → String → ServerEffect Unit
changePassword loggedUserId password = do
      hash ← SA.validatePassword password
      SSD.changePassword loggedUserId hash

terminateAccount ∷ Int → ServerEffect Unit
terminateAccount loggedUserId = SSD.terminateAccount loggedUserId

changePrivacySettings ∷ ∀ r. Int → PrivacySettings → BaseEffect { pool ∷ Pool | r } Unit
changePrivacySettings loggedUserId ps = SSD.changePrivacySettings loggedUserId ps

saveChatBackground ∷ Int → String → ServerEffect String
saveChatBackground loggedUserId base64 = do
      let sanitized = SS.sanitize $ DS.trim base64
      when (DS.null sanitized) <<< RE.throw $ BadRequest { reason: "invalid image" }
      fileName ← SF.saveBase64File sanitized
      SSD.saveChatBackground loggedUserId fileName
      pure $ SP.resourcePath (Left $ Upload fileName) Ignore