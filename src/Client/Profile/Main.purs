module Client.Profile.Main where

import Prelude
import Shared.IM.Types

import Client.Common.DOM (setChatExperiment)
import Client.Common.File as CCF
import Client.Profile.Update as CPU
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame.Application.Effectful as FAE
import Flame.Subscription as FS
import Shared.Options.MountPoint (imId, profileId)
import Shared.Profile.Types
import Shared.Profile.View as SPV
import Web.DOM.ParentNode (QuerySelector(..))

main ∷ Effect Unit
main = do
      FAE.resumeMount (QuerySelector ("#" <> show ProfileEditionForm)) profileId
            { view: SPV.view
            , subscribe: [ FS.onCustomEvent setChatExperiment SetProfileChatExperiment ]
            , init: Nothing
            , update: CPU.update
            }
      --a pain, but a chat experiment might be going on before loading the modal
      FS.send imId AskChatExperiment
      --avatar changes
      input ← CPU.getFileInput
      CCF.setUpFileChange (Save <<< Avatar <<< Just) input profileId
