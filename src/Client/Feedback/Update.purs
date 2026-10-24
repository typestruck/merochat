module Client.Feedback.Update (update) where

import Prelude

import Client.AppId (imAppId)
import Client.Lynx.Image as CLI
import Client.Network (routes)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as EA
import Effect.Class as EC
import Flame (Update)
import Flame.Subscription as FS
import Shared.Feedback.Types (FeedbackMessage(..), FeedbackModel, Status(..))
import Shared.Im.Types as SIT
import Shared.Modal (Modal(..), ScreenModal(..))
import Shared.Network (RequestStatus(..))
import Shared.Network as SN

update ∷ Update FeedbackModel FeedbackMessage
update model =
      case _ of
            ToggleVisibility modal → toggleVisibility model modal
            SetComments input → setComments input model
            SelectScreenshot → model /\ [ map (map SetScreenshot) CLI.pick ]
            SetScreenshot base64 → setScreenshot base64 model
            SendFeedback → sendFeedback model
            AfterSendFeedback status → afterSendFeedback status model

toggleVisibility ∷ FeedbackModel → Modal → FeedbackModel /\ Array (Aff (Maybe FeedbackMessage))
toggleVisibility model = case _ of
      HideModal → model /\ [ hide ]
      Screen modal → model { visible = modal == ShowFeedback } /\ []
      _ → model /\ []
      where
      hide = EC.liftEffect do
            FS.send imAppId <<< SIT.SpecialRequest $ SIT.ToggleModal HideModal
            pure Nothing

setComments ∷ String → FeedbackModel → FeedbackModel /\ Array (Aff (Maybe FeedbackMessage))
setComments input model =
      model
            { comments = input
            } /\ []

setScreenshot ∷ String → FeedbackModel → FeedbackModel /\ Array (Aff (Maybe FeedbackMessage))
setScreenshot base64 model =
      model
            { screenshot = Just base64
            } /\ []

sendFeedback ∷ FeedbackModel → FeedbackModel /\ Array (Aff (Maybe FeedbackMessage))
sendFeedback model = case DS.trim model.comments of
      "" → model { feedbackStatus = Just NoComments } /\ []
      trimmed → model { loading = true } /\ [ send trimmed ]
      where
      send trimmed = do
            response ← routes.feedback.send { body: { comments: trimmed, screenshot: model.screenshot } }
            case response of
                  Right _ → pure <<< Just <<< AfterSendFeedback $ Just Success
                  Left err → pure <<< Just <<< AfterSendFeedback <<< Just <<< Failure $ SN.errorMessage err

afterSendFeedback ∷ Maybe RequestStatus → FeedbackModel → FeedbackModel /\ Array (Aff (Maybe FeedbackMessage))
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
