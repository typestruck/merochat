module Client.Feedback.Update (update) where

import Prelude
import Shared.Feedback.Types (FeedbackMessage(..), FeedbackModel, Status(..))

import Client.AppId (feedbackAppId)
import Client.File as CCF
import Client.Network (request)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as EA
import Flame (Update)
import Shared.Modal.Types (ScreenModal(..))
import Shared.Network (RequestStatus(..))
import Shared.Network as SN
import Web.Event.Internal.Types (Event)

update ∷ Update FeedbackModel FeedbackMessage
update model =
      case _ of
            ToggleVisibility modal → toggleVisibility modal model
            SetComments input → setComments input model
            BeforeSetScreenshot event -> beforeSetScreenshot event model
            SetScreenshot base64 → setScreenshot base64 model
            SendFeedback → sendFeedback model
            AfterSendFeedback status → afterSendFeedback status model

toggleVisibility ∷ ScreenModal → FeedbackModel → FeedbackModel /\ Array (Aff (Maybe FeedbackMessage))
toggleVisibility modal model = model { visible = modal == ShowFeedback } /\ []

setComments :: String -> FeedbackModel → FeedbackModel /\ Array (Aff (Maybe FeedbackMessage))
setComments input model =
      model
            { comments = input
            } /\ []

beforeSetScreenshot ∷ Event → FeedbackModel → FeedbackModel /\ Array (Aff (Maybe FeedbackMessage))
beforeSetScreenshot event model = model /\ [ before ]
      where
      before = do
            CCF.compressImage feedbackAppId event false (\_ _ b → SetScreenshot b)
            pure Nothing

setScreenshot :: String -> FeedbackModel → FeedbackModel /\ Array (Aff (Maybe FeedbackMessage))
setScreenshot base64 model =
      model
            { screenshot = Just base64
            } /\ []

sendFeedback :: FeedbackModel → FeedbackModel /\ Array (Aff (Maybe FeedbackMessage))
sendFeedback model = case DS.trim model.comments of
      "" → model { feedbackStatus = Just NoComments } /\ []
      trimmed → model { loading = true } /\ [ send trimmed ]
      where
      send trimmed = do
            response ← request.feedback.send { body: { comments: trimmed, screenshot: model.screenshot } }
            case response of
                  Right _ → pure <<< Just <<< AfterSendFeedback $ Just Success
                  Left err → pure <<< Just <<< AfterSendFeedback <<< Just <<< Failure $ SN.errorMessage err

afterSendFeedback :: Maybe RequestStatus -> FeedbackModel → FeedbackModel /\ Array (Aff (Maybe FeedbackMessage))
afterSendFeedback status model = case status of
      Nothing → model { feedbackStatus = Nothing } /\ []
      Just (Failure _) → model { loading = false, feedbackStatus = Request <$> status } /\ []
      Just Success →
            model
                  { loading = false
                  , feedbackStatus = Request <$> status
                  , comments = ""
                  , screenshot = Nothing
                  } /\
                  [ after
                  ]
      where
      after = do
            EA.delay $ Milliseconds 4000.0
            pure <<< Just $ AfterSendFeedback Nothing
