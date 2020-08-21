module  Server.Settings.Handler where

import Server.Types
import Shared.Types

import Run as R
import Server.Settings.Action as SSA
import Server.Settings.Template as SST

settings :: { guards :: { loggedUserID :: PrimaryKey } } -> ServerEffect String
settings { guards: { loggedUserID } } = R.liftEffect SST.template

accountEmail :: { guards :: { loggedUserID :: PrimaryKey }, body :: String } -> ServerEffect Ok
accountEmail { guards: { loggedUserID }, body } = SSA.changeEmail loggedUserID body

accountPassword :: { guards :: { loggedUserID :: PrimaryKey }, body :: String } -> ServerEffect Ok
accountPassword { guards: { loggedUserID }, body } = SSA.changePassword loggedUserID body

accountTerminate :: forall r. { guards :: { loggedUserID :: PrimaryKey } | r } -> ServerEffect Ok
accountTerminate { guards: { loggedUserID } } = SSA.terminateAccount loggedUserID

