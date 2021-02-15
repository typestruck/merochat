module Server.Backer.Handler where

import Prelude
import Server.Types

import Server.Backer.Template as SBT
import Server.Response as SR

backer :: forall r. { | r  } -> ServerEffect Html
backer _ = SR.serveTemplate $ SBT.template
