module Server.Settings.Router where

import Prelude

import HTTPure (Request)
import Run as R
import Run.Reader as RR
import Server.Settings.Database as SSD
import Server.Settings.Template as SST
import Server.Response as SRR
import Server.Router.Session as SRS
import Server.Types (ResponseEffect)
import Server.Settings.Action as SSA
import Shared.Unsafe as SU

profile :: Request -> ResponseEffect
profile { path } = SRS.ifLogged path do
        contents <- R.liftEffect SST.template
        SRR.json' $ JSONString contents