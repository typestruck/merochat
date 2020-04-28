module Server.Profile.Action where

import Prelude

import Shared.Types
import Server.Types
import Shared.Profile.Types

saveProfile :: ProfileUser -> ServerEffect Ok
saveProfile profileUser = do
        pure Ok