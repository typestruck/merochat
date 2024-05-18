module Shared.Feedback.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Shared.Network (RequestStatus)

data Status
      = NoComments
      | Request RequestStatus

type FeedbackModel =
      { feedbackStatus ∷ Maybe Status
      , comments ∷ String
      , screenshot ∷ Maybe String
      , loading ∷ Boolean
      }

data FeedbackMessage
      = SetComments String
      | SetScreenshot String
      | SetFeedbackStatus (Maybe RequestStatus)
      | SendFeedback

derive instance Eq Status

derive instance Generic Status _

instance EncodeJson Status where
      encodeJson = DAEGR.genericEncodeJson

instance DecodeJson Status where
      decodeJson = DADGR.genericDecodeJson
