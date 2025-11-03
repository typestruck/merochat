module Server.Help.Handler where

import Prelude
import Server.Effect

import Effect.Class as EC
import Server.Help.Template as SHT
import Shared.Html (Html)

help ∷ ∀ r. { | r } → ServerEffect Html
help _ = EC.liftEffect SHT.template
