module Shared.Router.Default where

import Prelude
import Shared.Types

import Data.DateTime (DateTime(..), Time(..))
import Data.DateTime as DD
import Data.Enum (class BoundedEnum)
import Data.Enum as DE
import Data.Maybe (Maybe(..))
import Shared.PrimaryKey as SP
import Shared.Router as SR
import Shared.Unsafe as SU

dummyID :: PrimaryKey
dummyID = SP.fromInt 0

login :: String
login = SR.fromRoute $ Login { next: Nothing }

contacts :: String
contacts = SR.fromRouteToPath $ Contacts { skip: 0 }

history :: String
history = SR.fromRouteToPath $ History { skip: 0, with: dummyID }

generate :: String
generate = SR.fromRouteToPath $ Generate { what: Name }

recover :: String
recover = SR.fromRoute $ Recover { token: Nothing }

singleContact :: String
singleContact = SR.fromRouteToPath $ SingleContact { id: dummyID }

blockUser :: String
blockUser = SR.fromRouteToPath $ Block { id: dummyID }

missedMessages :: String
missedMessages = SR.fromRouteToPath $ MissedMessages {
        since: DateTime (DD.canonicalDate (toEnum' 1970) (toEnum' 1) (toEnum' 1)) $ Time (toEnum' 0) (toEnum' 0) (toEnum' 0) (toEnum' 0)
}
      where toEnum' :: forall a. BoundedEnum a => Int -> a
            toEnum' = SU.fromJust <<< DE.toEnum