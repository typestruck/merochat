module Server.Terms.Handler where

import Server.Types

import Server.Response as SR
import Server.Terms.Template as STT

terms :: forall r. { | r} -> ServerEffect Html
terms _ = SR.serveTemplate STT.template