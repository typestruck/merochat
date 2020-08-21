module Client.Common.Logout where

import Prelude


import Client.Common.Location as CCL
import Data.Maybe (Maybe(..))
import Effect (Effect)



logout :: Effect Unit
logout = do
        --CCL.setLocation Landing
        pure unit
