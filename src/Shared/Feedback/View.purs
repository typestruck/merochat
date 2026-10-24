module Shared.Feedback.View where

import Prelude

import Data.Maybe (Maybe(..))
import Shared.Modal (Modal(..))
import Shared.Modal as SM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Data.Maybe as DM
import Shared.Element (ElementId(..))
import Shared.Extra (br)
import Shared.Extra as SE
import Shared.Feedback.Types (FeedbackMessage(..), FeedbackModel, Status(..))
import Shared.Network (RequestStatus(..))

view ∷ FeedbackModel → Html FeedbackMessage
view model =
      HE.div [ HA.id $ show FeedbackForm, HA.class' { hidden: not model.visible } ]
            [ SM.header "Send feedback" $ ToggleVisibility HideModal
            , HE.div [ HA.class' "center duller" ]
                    [ HE.span_ [ HE.text "Use the form bellow to report any issues," ]
                    , br
                    , HE.span_ [ HE.text "send suggestions or shoot any questions you might have" ]
                    ]
            , HE.div [ HA.class' "extra-padding" ]
                    [ HE.label_ [ HE.text "What would you like to say?" ]
                    , HE.textarea' [ HA.class' "comments modal-input", HA.onInput SetComments, HA.value model.comments ]
                    , HE.div [ HA.class' { "error-message": true, hidden: model.feedbackStatus /= Just NoComments } ] [ HE.text "Field is mandatory" ]
                    ]
            , HE.div_
                    [ HE.label_ [ HE.text "Optionally, include a screenshot" ]
                    , HE.button [ HA.onClick SelectScreenshot, HA.class' "modal-input" ] [ HE.text "Choose screenshot" ]
                    , if DM.isJust model.screenshot then HE.img [ HA.class' "feedback-screenshot", HA.src $ DM.fromMaybe "" model.screenshot ] else HE.div_ []
                    ]
            , HE.div [ HA.class' "buttons" ]
                    [ if model.loading then
                            HE.div' [ HA.class' "loading" ]
                      else
                            HE.button [ HA.class' "green-button", HA.onClick SendFeedback ] [ HE.text $ "Send" ]
                    , HE.div [ HA.class' { "error-message": true, hidden: not isFailure } ] [ HE.text "Could not send feedback. Please try again" ]
                    , HE.div [ HA.class' { "success-message": true, hidden: model.feedbackStatus /= Just (Request Success) } ] [ HE.text "Feedback sent!" ]
                    ]
            , HE.div [ HA.class' "duller center" ]
                    [ br
                    , SE.nestedText []
                            [ HE.text "You can also reach out at "
                            , SE.textLink [ HA.href "https://reddit.com/r/MeroChat", SE.externalLink "https://reddit.com/r/MeroChat", HA.target "_blank" ] [ HE.text "r/MeroChat" ]
                            , HE.text " and "
                            , SE.textLink [ HA.href "https://twitter.com/MeroChat", SE.externalLink "https://twitter.com/MeroChat", HA.target "_blank" ] [ HE.text "@MeroChat" ]
                            ]
                    ]
            ]
      where
      isFailure = case model.feedbackStatus of
            Just (Request (Failure _)) → true
            _ → false
