module Server.Admin.Handler where

import Prelude

import Server.Admin.Action as SAA
import Server.Ok (Ok, ok)
import Server.Types (ServerEffect)

ban ∷ { guards ∷ { loggedUserId ∷ Int }, query ∷ { id ∷ Int, secret ∷ String } } → ServerEffect Ok
ban { guards: { loggedUserId }, query } = do
      SAA.ban loggedUserId query
      pure ok