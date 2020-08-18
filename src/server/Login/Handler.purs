module Server.Login.Handler where

import Prelude
import Server.Types

import Data.Maybe (Maybe)
import Server.Login.Action as SLA
import Server.Login.Template as SLT
import Server.R as SR
import Shared.Types (RegisterLogin(..))

login :: forall r. { | r } -> ServerEffect Html
login _ = SR.serveTemplate SLT.template

login' :: forall r. { query :: { next :: Maybe String }, body :: RegisterLogin | r } -> ServerEffect Html
login' { query: { next }, body } = Html <$> SLA.login body
