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
import Shared.Types (JSONResponse(..), PrimaryKey(..))
import Shared.Unsafe as SU

settings :: Request -> ResponseEffect
settings request = do
        void $ SRS.checkLogin request
        contents <- R.liftEffect SST.template
        SRR.json' $ JSONResponse contents

changeEmail :: Request -> ResponseEffect
changeEmail request@{ body } = do
        userID <- SRS.checkLogin request
        SRR.json body (SSA.changeEmail userID)

--REFACTOR: abstract this workflow of doing a json action after getting the logged id
changePassword :: Request -> ResponseEffect
changePassword request@{ body } = do
        userID <- SRS.checkLogin request
        SRR.json body (SSA.changePassword userID)

terminateAccount :: Request -> ResponseEffect
terminateAccount request = do
        userID <- SRS.checkLogin request
        ok <- SSA.terminateAccount userID
        SRR.json' ok

