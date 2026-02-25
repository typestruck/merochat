module Server.Praise.Handler where

import Prelude

import Debug (spy)
import Server.Effect (ServerEffect)
import Server.Praise.Action as SPA
import Shared.Praise (Praise, PraisedFor, PraiseDisplay)

praise ∷ { guards ∷ { loggedUserId ∷ Int }, query ∷ { praised ∷ Int } } → ServerEffect PraiseDisplay
praise request = SPA.presentPraise request.guards.loggedUserId request.query.praised

post ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { userId ∷ Int, for ∷ Array PraisedFor } } → ServerEffect { allowed ∷ Boolean }
post request = do
      allowed ← SPA.savePraise request.guards.loggedUserId request.body.userId request.body.for
      pure { allowed }

