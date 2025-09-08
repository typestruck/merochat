module Shared.Feedback.View where

import Prelude
import Shared.Feedback.Types

import Data.Maybe (Maybe(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Element (ElementId(..))
import Shared.Network (RequestStatus(..))

view ∷ FeedbackModel → Html FeedbackMessage
view model@{ feedbackStatus, loading, comments } =
      HE.div [ HA.id $ show FeedbackForm, HA.class' { hidden: not model.visible } ]
            [ HE.div [HA.class' "center duller"]
                    [ HE.span_ [HE.text "Use the form bellow to report any issues,"]
                    , HE.br
                    , HE.span_ [HE.text"send suggestions or shoot any questions you might have"]
                    ]
            , HE.div [HA.class' "extra-padding"]
                    [ HE.label_ [HE.text "What would you like to say?"]
                    , HE.textarea' [ HA.class' "comments modal-input", HA.onInput SetComments, HA.value comments ]
                    , HE.div [ HA.class' { "error-message": true, hidden: feedbackStatus /= Just NoComments } ] [HE.text "Field is mandatory"]
                    ]
            , HE.div_
                    [ HE.label_ [HE.text "Optionally, include a screenshot"]
                    , HE.input [ HA.id $ show ScreenshotInput, HA.type' "file", HA.class' "modal-input", HA.accept ".png, .jpg, .jpeg, .tif, .tiff, .bmp" ]
                    ]
            , HE.div [HA.class' "buttons"]
                    [ if loading then
                            HE.div' [HA.class' "loading"]
                      else
                            HE.input [ HA.type' "button", HA.class' "green-button", HA.value "Send", HA.onClick SendFeedback ]
                    , HE.div [ HA.class' { "error-message": true, hidden: not isFailure } ] [HE.text "Could not send feedback. Please try again"]
                    , HE.div [ HA.class' { "success-message": true, hidden: feedbackStatus /= Just (Request Success) } ] [HE.text "Feedback sent!"]
                    ]
            , HE.div [HA.class' "duller center"]
                    [ HE.br
                    , HE.span_ [HE.text "You can also reach out at "]
                    , HE.a [ HA.href "https://reddit.com/r/MeroChat", HA.target "_blank" ] [HE.text "r/MeroChat"]
                    , HE.span_ [HE.text " and "]
                    , HE.a [ HA.href "https://twitter.com/MeroChat", HA.target "_blank" ] [HE.text "@MeroChat"]
                    ]
            ]
      where
      isFailure = case feedbackStatus of
            Just (Request (Failure _)) → true
            _ → false
