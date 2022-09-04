module Server.Recover.Handler where

import Prelude
import Server.Types

import Data.Maybe (Maybe)
import Server.Ok (Ok, ok)
import Server.Recover.Action as SRA
import Server.Recover.Template as SRT
import Server.Response as SR
import Shared.Account (ResetPassword, RecoverAccount)
import Shared.Html (Html(..))

recover ∷ ∀ r. { query ∷ { token ∷ Maybe String } | r } → ServerEffect Html
recover { query: { token } } = SR.serveTemplate $ SRT.template token

recoverAccount ∷ ∀ r. { body ∷ RecoverAccount | r } → ServerEffect Ok
recoverAccount { body } = do
      SRA.recover body
      pure ok

reset ∷ ∀ r. { body ∷ ResetPassword | r } → ServerEffect Ok
reset { body } = do
      SRA.reset body
      pure ok