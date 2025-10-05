module Server.Recover.Handler where

import Prelude
import Server.Effect

import Data.Maybe (Maybe)
import Server.Ok (Ok, ok)
import Server.Recover.Action as SRA
import Server.Recover.Template as SRT
import Server.Response as SR
import Shared.Account (ResetPassword, EmailCaptcha)
import Shared.Html (Html(..))

recover ∷ ∀ r. { guards ∷ { checkAnonymous ∷ Unit }, query ∷ { token ∷ Maybe String } | r } → ServerEffect Html
recover { query: { token } } = SR.serveTemplate $ SRT.template token

recoverAccount ∷ { guards ∷ { checkAnonymous ∷ Unit }, body ∷ EmailCaptcha } → ServerEffect Ok
recoverAccount { body } = do
      SRA.recover body
      pure ok

reset ∷ { guards ∷ { checkAnonymous ∷ Unit }, body ∷ ResetPassword } → ServerEffect Ok
reset { body } = do
      SRA.reset body
      pure ok