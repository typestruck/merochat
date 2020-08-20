module Client.Common.Logout where

import Prelude


import Client.Common.Location as CCL
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Shared.Router as SRO
import Shared.Types (Route(..))

logout :: Effect Unit
logout = do
        CCL.setLocation Landing
