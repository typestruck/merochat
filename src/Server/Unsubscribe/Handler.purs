module Server.Unsubscribe.Handler where

import Prelude

import Effect.Class as EC
import Server.Effect (ServerEffect)
import Server.NotFound.Template as SUN
import Server.Unsubscribe.Action as SUA
import Server.Unsubscribe.Template as SUT
import Shared.Html (Html)

unsubscribe ∷ { query ∷ { emailId ∷ String } } → ServerEffect Html
unsubscribe { query: { emailId } } = do
      unsubbed ← SUA.unsubscribe emailId
      EC.liftEffect $ if unsubbed then SUT.template else SUN.template
