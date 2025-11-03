module Server.NotFound.Handler where

import Server.Effect

import Effect.Class as EC
import Server.NotFound.Template as SNT
import Shared.Html (Html)

notFound ∷ ∀ r. { | r } → ServerEffect Html
notFound _ = EC.liftEffect SNT.template
