module Server.Privacy.Handler where

import Server.Types

import Server.Response as SR
import Server.Privacy.Template as SPT

privacy :: forall r. { | r} -> ServerEffect Html
privacy _ = SR.serveTemplate SPT.template