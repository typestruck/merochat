module Server.NotFound.Handler where

import Prelude
import Server.Types
import Shared.Types

import Data.Tuple (Tuple(..))
import Payload.Headers as PH
import Payload.ResponseTypes (Empty(..), Response)
import Server.R as SR
import Run.Reader as RR
import Server.NotFound.Template as SNT

notFound :: forall r. { | r } -> ServerEffect Html
notFound _ = SR.serveTemplate SNT.template

