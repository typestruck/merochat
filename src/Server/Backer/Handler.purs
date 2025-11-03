module Server.Backer.Handler where

import Prelude
import Server.Effect

import Effect.Class as EC
import Server.Backer.Template as SBT
import Shared.Html (Html)

backer ∷ ∀ r. { | r } → ServerEffect Html
backer _ = EC.liftEffect SBT.template
