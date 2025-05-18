module Client.Feedback.Main where

import Prelude

import Client.Common.File as CCF
import Client.Feedback.Update as CFU
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.Element (ElementId(..))
import Shared.Feedback.Types (FeedbackMessage(..))
import Shared.Feedback.View as SFV
import Shared.Options.MountPoint (feedbackId)

main ∷ Effect Unit
main = do
      F.resumeMount (QuerySelector $ "#" <> show FeedbackForm) feedbackId
            { view: SFV.view
            , update: CFU.update
            , subscribe: []
            , init: []
            }
      --file changes
      input ← CFU.getFileInput
      CCF.setUpFileChange (\_ _ b → SetScreenshot b) input feedbackId