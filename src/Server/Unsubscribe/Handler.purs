module Server.Unsubscribe.Handler where

import Prelude

import Effect.Class as EC
import Server.Effect (ServerEffect)
import Server.NotFound.Template as SUN
import Server.Unsubscribe.Action as SUA
import Server.Unsubscribe.Template as SUT
import Shared.Html (Html)

unsubscribe ∷ { query ∷ { token ∷ String } } → ServerEffect Html
unsubscribe routes = do
      unsubbed ← SUA.unsubscribe routes.query.token
      EC.liftEffect $ if unsubbed then SUT.template else SUN.template
