module Server.Elsewhere.Handler where

import Prelude

import Effect.Class as EC
import Server.Effect (ServerEffect)
import Server.Elsewhere.Template as SET
import Shared.Html (Html)

elsewhere ∷ ∀ r. { | r } → ServerEffect Html
elsewhere _ = EC.liftEffect SET.template