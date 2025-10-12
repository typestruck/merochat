-- | Entry point of the feedback modal page

module Client.Feedback.Main
      ( main
      ) where

import Prelude

import Client.AppId (feedbackAppId)
import Client.Common.File as CCF
import Client.Feedback.Update as CFU
import Effect (Effect)
import Flame as F
import Flame.Subscription as FS
import Shared.Element (ElementId(..))
import Shared.Element as SE
import Shared.Feedback.Types (FeedbackMessage(..))
import Shared.Feedback.View as SFV
import Shared.Im.EventTypes (modalVisible)

main ∷ Effect Unit
main = do
      void $ F.resumeMount (SE.toQuerySelector FeedbackForm) feedbackAppId
            { view: SFV.view
            , update: CFU.update
            , subscribe: [ FS.onCustomEvent modalVisible ToggleVisibility ]
            }
      --file changes
      input ← CFU.getFileInput
      CCF.setUpFileChange (\_ _ b → SetScreenshot b) input feedbackAppId