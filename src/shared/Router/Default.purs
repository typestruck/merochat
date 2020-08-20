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
import Shared.DateTime(zeroTime)

dummyID :: PrimaryKey
dummyID = SP.fromInt 0

login :: String
login = SR.fromRoute $ Login { next: Nothing }

generate :: String
generate = SR.fromRouteToPath $ Generate { what: Name }

recover :: String
recover = SR.fromRoute $ Recover { token: Nothing }

blockUser :: String
blockUser = SR.fromRouteToPath $ Block { id: dummyID }

missedMessages :: String
missedMessages = SR.fromRouteToPath $ MissedMessages {
        since: DateTime (DD.canonicalDate (SU.toEnum 1970) (SU.toEnum 1) (SU.toEnum 1)) zeroTime
}