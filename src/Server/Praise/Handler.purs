module Server.Praise.Handler where

import Prelude

import Debug (spy)
import Server.Effect (ServerEffect)
import Server.Praise.Action as SPA
import Shared.Praise (PraisedFor)

save ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { userId ∷ Int, for ∷ Array PraisedFor } } → ServerEffect { allowed ∷ Boolean }
save request = do
      allowed ← SPA.savePraise request.guards.loggedUserId request.body.userId request.body.for
      pure { allowed }

