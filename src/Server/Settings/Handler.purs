module  Server.Settings.Handler where

import Prelude
import Server.Types
import Shared.Types

import Payload.ResponseTypes (Response)
import Run as R
import Server.Logout as SL
import Server.Ok (ok)
import Server.Settings.Action as SSA
import Server.Settings.Template as SST
import Shared.Routes (routes)

settings :: { guards :: { loggedUserID :: PrimaryKey } } -> ServerEffect String
settings { guards: { loggedUserID } } = R.liftEffect SST.template

accountEmail :: { guards :: { loggedUserID :: PrimaryKey }, body :: String } -> ServerEffect Ok
accountEmail { guards: { loggedUserID }, body } = SSA.changeEmail loggedUserID body

accountPassword :: { guards :: { loggedUserID :: PrimaryKey }, body :: String } -> ServerEffect (Response Ok)
accountPassword { guards: { loggedUserID }, body } = do
      SSA.changePassword loggedUserID body
      pure $ SL.logout (routes.login.get { }) ok

accountTerminate :: forall r. { guards :: { loggedUserID :: PrimaryKey } | r } -> ServerEffect (Response Ok)
accountTerminate { guards: { loggedUserID } } = do
      SSA.terminateAccount loggedUserID
      pure $ SL.logout (routes.landing {}) ok

