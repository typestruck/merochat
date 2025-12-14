module Server.Backer.Handler where

import Server.Effect (ServerEffect)

import Effect.Class as EC
import Server.Backer.Template as SBT
import Shared.Html (Html)

backer ∷ ServerEffect Html
backer = EC.liftEffect SBT.template
