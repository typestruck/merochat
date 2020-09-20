module Server.NotFound.Handler where

import Server.Types

import Server.NotFound.Template as SNT
import Server.Response as SR

notFound :: forall r. { | r } -> ServerEffect Html
notFound _ = SR.serveTemplate SNT.template

