module Client.Common.Types where

import Prelude

data RequestStatus = Success | Fail

derive instance eqRequestStatus :: Eq RequestStatus
