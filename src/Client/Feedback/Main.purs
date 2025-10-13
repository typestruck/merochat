-- | Entry point of the feedback modal page

module Client.Feedback.Main where

import Prelude

import Client.AppId (feedbackAppId)
import Client.Feedback.Update as CFU
import Effect (Effect)
import Flame as F
import Flame.Subscription as FS
import Shared.Element (ElementId(..))
import Shared.Element as SE
import Shared.Feedback.Types (FeedbackMessage(..))
import Shared.Feedback.View as SFV
import Client.EventTypes (modalVisible)

main ∷ Effect Unit
main = do
      void $ F.resumeMount (SE.toQuerySelector FeedbackForm) feedbackAppId
            { view: SFV.view
            , update: CFU.update
            , subscribe: [ FS.onCustomEvent modalVisible ToggleVisibility ]
            }