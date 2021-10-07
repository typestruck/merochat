module Server.Settings.Handler where

import Prelude
import Server.Types
import Shared.ContentType

import Payload.ResponseTypes (Response)
import Run as R
import Server.Logout as SL
import Server.Ok (Ok, ok)
import Server.Settings.Action as SSA
import Server.Settings.Database as SSD
import Server.Settings.Template as SST
import Shared.User (ProfileVisibility)

settings ∷ { guards ∷ { loggedUserID ∷ Int } } → ServerEffect String
settings { guards: { loggedUserID } } = do
      profileVisibility <- SSD.profileVisibility loggedUserID
      R.liftEffect $ SST.template profileVisibility

accountEmail ∷ { guards ∷ { loggedUserID ∷ Int }, body ∷ { email ∷ String } } → ServerEffect (Response Ok)
accountEmail { guards: { loggedUserID }, body: { email } } = do
      SSA.changeEmail loggedUserID email
      pure SL.expireCookies

accountPassword ∷ { guards ∷ { loggedUserID ∷ Int }, body ∷ { password ∷ String } } → ServerEffect (Response Ok)
accountPassword { guards: { loggedUserID }, body: { password } } = do
      SSA.changePassword loggedUserID password
      pure SL.expireCookies

accountTerminate ∷ ∀ r. { guards ∷ { loggedUserID ∷ Int } | r } → ServerEffect (Response Ok)
accountTerminate { guards: { loggedUserID } } = do
      SSA.terminateAccount loggedUserID
      pure SL.expireCookies

changeVisibility ∷ { guards ∷ { loggedUserID ∷ Int }, body :: ProfileVisibility } → ServerEffect Ok
changeVisibility { guards: { loggedUserID }, body } = do
      SSA.changeVisibility loggedUserID body
      pure ok