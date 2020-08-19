module Client.Common.Logout where

import Prelude


import Client.Common.Location as CCL
import Client.Common.Storage (tokenKey)
import Client.Common.Storage as CCS
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Shared.Router as SRO
import Shared.Types (Route(..))

logout :: Effect Unit
logout = do
        CCS.removeItem tokenKey
--        CCC.removeMelanchatCookie
        CCL.setLocation Landing
