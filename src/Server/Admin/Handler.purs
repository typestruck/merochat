module Server.Admin.Handler where

import Prelude

import Payload.ResponseTypes (Empty(..))
import Server.Admin.Action as SAA
import Server.Effect (ServerEffect)

ban ∷ { guards ∷ { loggedUserId ∷ Int }, query ∷ { id ∷ Int, secret ∷ String } } → ServerEffect Empty
ban { guards: { loggedUserId }, query } = do
      SAA.ban loggedUserId query
      pure Empty