module Shared.Network where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Generic.Rep (class Generic)

data RequestStatus = Success | Failure

derive instance Eq RequestStatus

derive instance Generic RequestStatus _

instance EncodeJson RequestStatus where
      encodeJson = DAEGR.genericEncodeJson

instance DecodeJson RequestStatus where
      decodeJson = DADGR.genericDecodeJson
