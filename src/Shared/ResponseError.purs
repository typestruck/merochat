module Shared.ResponseError where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic as DGRS

-- | Errors that should be reported back to the user
data ResponseError
      = BadRequest { reason ∷ String }
      | InternalError { reason ∷ String, context ∷ Maybe DatabaseError }
      | ExpiredSession

data DatabaseError = MissingForeignKey

instance Show ResponseError where
      show = DGRS.genericShow
instance Show DatabaseError where
      show = DGRS.genericShow

derive instance Generic ResponseError _
derive instance Generic DatabaseError _

derive instance Eq DatabaseError

instance EncodeJson DatabaseError where
      encodeJson = DAEGR.genericEncodeJson

instance DecodeJson DatabaseError where
      decodeJson = DADGR.genericDecodeJson