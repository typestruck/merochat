module Client.Feedback.Main where

import Prelude

import Client.Common.Dom as CCD
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
            , subscribe: []
            , init: []
            , update: CFU.update
            }
      --file changes
      input ← CCD.unsafeGetElementById ScreenshotInput
      CCF.setUpFileChange SetScreenshot input feedbackId