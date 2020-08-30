module Server.Help.Handler where

import Server.Types

import Server.Response as SR
import Server.Help.Template as SHT

help :: forall r. { | r} -> ServerEffect Html
help _ = SR.serveTemplate SHT.template