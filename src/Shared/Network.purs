module Shared.Network where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Generic.Rep (class Generic)
import Payload.Client (ClientError(..))
import Payload.ResponseTypes (Response(..))

data RequestStatus = Success | Failure String

derive instance Eq RequestStatus

derive instance Generic RequestStatus _

instance DecodeJson RequestStatus where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson RequestStatus where
      encodeJson = DAEGR.genericEncodeJson


errorMessage ∷ ClientError → String
errorMessage = case _ of
      DecodeError _ → "Server sent an unexpected response"
      StatusError { response: Response { body } } → body
      RequestError { message } → message
