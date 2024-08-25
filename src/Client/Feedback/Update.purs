module Client.Feedback.Update where

import Prelude
import Shared.Feedback.Types

import Client.Common.Dom as CCD
import Client.Common.Network (request)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Flame (ListUpdate)
import Flame as F
import Shared.Element (ElementId(..))
import Shared.Network (RequestStatus(..))
import Shared.Network as SN
import Shared.Unsafe as SU
import Web.DOM (Element)
import Web.HTML.HTMLInputElement as WHI

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
                              } /\
                              [ do
                                      liftEffect do
                                            input ← getFileInput
                                            WHI.setValue "" <<< SU.fromJust $ WHI.fromElement input
                                      EA.delay $ Milliseconds 4000.0
                                      pure <<< Just $ SetFeedbackStatus Nothing
                              ]
                  Nothing → F.noMessages model { feedbackStatus = Nothing }
            SendFeedback → case DS.trim comments of
                  "" → F.noMessages model { feedbackStatus = Just NoComments }
                  trimmed → model { loading = true } /\
                        [ do
                                response ← request.feedback.send { body: { comments: trimmed, screenshot } }
                                case response of
                                      Right _ → pure <<< Just <<< SetFeedbackStatus $ Just Success
                                      Left err → pure <<< Just <<< SetFeedbackStatus <<< Just <<< Failure $ SN.errorMessage err
                        ]

getFileInput ∷ Effect Element
getFileInput = CCD.unsafeGetElementById ScreenshotInput