module Shared.Router.Default where

import Data.Maybe (Maybe(..))
import Shared.PrimaryKey as SP
import Shared.Router as SR
import Shared.Types (Generate(..), Route(..))

login :: String
login = SR.fromRoute (Login { next: Nothing })

contacts :: String
contacts = SR.fromRouteToPath (Contacts { skip: 0 })

history :: String
history = SR.fromRouteToPath (History { skip: 0, with: SP.fromInt 0 })

generate :: String
generate = SR.fromRouteToPath (Generate { what: Name })

recover :: String
recover = SR.fromRoute (Recover { token: Nothing })