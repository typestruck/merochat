module Shared.Feedback.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADG
import Data.Argonaut.Encode.Generic as DAEG
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Flame.Html.Event (Event)
import Shared.Modal (ScreenModal)
import Shared.Network (RequestStatus)

-- | Used for validation / error and success messages
data Status
      = NoComments
      | Request RequestStatus

type FeedbackModel =
      { feedbackStatus ∷ Maybe Status
      , comments ∷ String
      , visible ∷ Boolean
      , screenshot ∷ Maybe String
      , loading ∷ Boolean
      }

data FeedbackMessage
      = SetComments String
      | BeforeSetScreenshot Event
      | SetScreenshot String
      | ToggleVisibility ScreenModal
      | SendFeedback
      | AfterSendFeedback (Maybe RequestStatus)

derive instance Eq Status

derive instance Generic Status _

instance EncodeJson Status where
      encodeJson = DAEG.genericEncodeJson

instance DecodeJson Status where
      decodeJson = DADG.genericDecodeJson
