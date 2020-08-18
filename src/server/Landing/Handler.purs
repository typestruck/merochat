module Server.Landing.Handler where

import Prelude
import Server.Types
import Server.Landing.Template as SLT
import Server.R as SR

landing :: forall r. { | r } -> ServerEffect Html
landing _ = SR.serveTemplate SLT.template