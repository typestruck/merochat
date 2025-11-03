module Server.Recover.Handler where

import Prelude

import Data.Maybe (Maybe)
import Effect.Class as EC
import Payload.ResponseTypes (Empty(..))
import Server.Effect (ServerEffect)
import Server.Recover.Action as SRA
import Server.Recover.Template as SRT
import Server.Response as SR
import Shared.Account (ResetPassword, EmailCaptcha)
import Shared.Html (Html)

recover ∷ ∀ r. { guards ∷ { checkAnonymous ∷ Unit }, query ∷ { token ∷ Maybe String } | r } → ServerEffect Html
recover { query: { token } } = EC.liftEffect $ SRT.template token

recoverAccount ∷ { guards ∷ { checkAnonymous ∷ Unit }, body ∷ EmailCaptcha } → ServerEffect Empty
recoverAccount { body } = do
      SRA.recover body
      pure Empty

reset ∷ { guards ∷ { checkAnonymous ∷ Unit }, body ∷ ResetPassword } → ServerEffect Empty
reset { body } = do
      SRA.reset body
      pure Empty