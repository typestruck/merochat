module Server.App.Handler where

import Effect.Class as EC
import Server.Effect (ServerEffect)
import Server.App.Template as SAT
import Shared.Html (Html)

app ∷ _ → ServerEffect Html
app _ = EC.liftEffect SAT.template