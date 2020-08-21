module Server.Recover.Handler where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe (Maybe)
import Server.Recover.Action as SRA
import Server.Recover.Template as SRT
import Server.Response as SR

recover :: forall r. { query :: { token :: Maybe String } | r } -> ServerEffect Html
recover { query: { token } } = SR.serveTemplate $ SRT.template token

recoverAccount :: forall r. { body :: RecoverAccount | r } -> ServerEffect Ok
recoverAccount { body } = SRA.recover body

reset :: forall r. { body :: ResetPassword | r } -> ServerEffect Ok
reset { body } = SRA.reset body