module Server.Settings.Router where

import Prelude

import HTTPure (Request)
import Run as R
import Run.Reader as RR
import Server.Response as SRR
import Server.Router.Session as SRS
import Server.Settings.Action as SSA
import Server.Settings.Database as SSD
import Server.Settings.Template as SST
import Server.Types (ResponseEffect)
import Shared.Types (JSONString(..), PrimaryKey(..))
import Shared.Unsafe as SU

settings :: Request -> ResponseEffect
settings { path } = SRS.ifLogged path do
        contents <- R.liftEffect SST.template
        SRR.json' $ JSONString contents

changeEmail :: Request -> ResponseEffect
changeEmail { path, body } = SRS.ifLogged path do
        userID <- SRS.loggedUserID
        SRR.json body (SSA.changeEmail userID)

changePassword :: Request -> ResponseEffect
changePassword { path, body } = SRS.ifLogged path do
        userID <- SRS.loggedUserID
        SRR.json body (SSA.changePassword userID)
