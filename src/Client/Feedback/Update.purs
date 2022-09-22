module Client.Feedback.Update where

import Prelude
import Shared.Feedback.Types

import Client.Common.Network (request)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as DS
import Debug (spy)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as EA
import Flame (ListUpdate, (:>))
import Flame as F
import Shared.Network (RequestStatus(..))
import Shared.Network as SN

update ∷ ListUpdate FeedbackModel FeedbackMessage
update model@{ comments, screenshot } =
      case _ of
            SetComments input → F.noMessages $ model
                  { comments = input
                  }
            SetScreenshot input → F.noMessages $ model
                  { screenshot = Just input
                  }
            SetFeedbackStatus status → case status of
                  Just (Failure _) → F.noMessages model { loading = false, feedbackStatus = Request <$> status }
                  Just Success →
                        model
                              { loading = false
                              , feedbackStatus = Request <$> status
                              , comments = ""
                              , screenshot = Nothing
                              } :>
                              [ do
                                      EA.delay $ Milliseconds 4000.0
                                      pure <<< Just $ SetFeedbackStatus Nothing
                              ]
                  Nothing → F.noMessages model { feedbackStatus = Nothing }
            SendFeedback → case DS.trim comments of
                  "" → F.noMessages model { feedbackStatus = Just NoComments }
                  trimmed → model { loading = true } :>
                        [ do
                                response ← request.feedback.send { body: { comments: trimmed, screenshot } }
                                case response of
                                      Right _ → pure <<< Just <<< SetFeedbackStatus $ Just Success
                                      Left err → pure <<< Just <<< SetFeedbackStatus <<< Just <<< Failure $ SN.errorMessage err
                        ]