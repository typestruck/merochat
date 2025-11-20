module Server.Settings.Handler where

import Prelude
import Server.Effect

import Data.Maybe (Maybe)
import Payload.ResponseTypes (Empty(..), Response)
import Run as R
import Server.Logout as SL
import Server.Settings.Action as SSA
import Server.Settings.Database as SSD
import Server.Settings.Template as SST
import Shared.Html (Html)
import Shared.Settings.Types (PrivacySettings)

settings ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect Html
settings { guards: { loggedUserId } } = do
      userSettings ← SSD.userSettings loggedUserId
      R.liftEffect $ SST.template userSettings

accountEmail ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { email ∷ String } } → ServerEffect (Response Empty)
accountEmail { guards: { loggedUserId }, body: { email } } = do
      SSA.changeEmail loggedUserId email
      pure SL.expireCookies

accountPassword ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { password ∷ String } } → ServerEffect (Response Empty)
accountPassword { guards: { loggedUserId }, body: { password } } = do
      SSA.changePassword loggedUserId password
      pure SL.expireCookies

accountTerminate ∷ ∀ r. { guards ∷ { loggedUserId ∷ Int } | r } → ServerEffect (Response Empty)
accountTerminate { guards: { loggedUserId } } = do
      SSA.terminateAccount loggedUserId
      pure SL.expireCookies

changePrivacy ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ PrivacySettings } → ServerEffect Empty
changePrivacy { guards: { loggedUserId }, body } = do
      SSA.changePrivacySettings loggedUserId body
      pure Empty

background ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { ownBackground ∷ Boolean, image ∷ Maybe String } } → ServerEffect String
background request = SSA.saveChatBackground request.guards.loggedUserId request.body.ownBackground request.body.image
