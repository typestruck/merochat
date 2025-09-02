module Client.Feedback.Main where

import Prelude

import Client.Common.File as CCF
import Client.Feedback.Update as CFU
import Effect (Effect)
import Web.DOM.ParentNode (QuerySelector(..))
import Flame as F
import Flame.Subscription as FS
import Shared.Im.EventTypes (modalVisible)
import Shared.Element (ElementId(..))
import Shared.Feedback.Types (FeedbackMessage(..))
import Shared.Feedback.View as SFV
import Shared.Options.MountPoint (feedbackId)

main ∷ Effect Unit
main = do
      void $ F.resumeMount (QuerySelector $ "#" <> show FeedbackForm) feedbackId
            { view: SFV.view
            , update: CFU.update
            , subscribe: [ FS.onCustomEvent modalVisible ToggleVisibility ]
            }
      --file changes
      input ← CFU.getFileInput
      CCF.setUpFileChange (\_ _ b → SetScreenshot b) input feedbackId