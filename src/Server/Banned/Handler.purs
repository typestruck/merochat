module Server.Banned.Handler where

import Prelude

import Effect.Class as EC
import Server.Banned.Template as SBT
import Server.Effect (ServerEffect)
import Shared.Html (Html)

banned ∷ ∀ r. { | r } → ServerEffect Html
banned _ = EC.liftEffect SBT.template