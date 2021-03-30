module Client.Profile.Main where

import Prelude
import Shared.Types

import Client.Common.DOM (setChatExperiment, askChatExperiment)
import Client.Common.File as CCF
import Client.Profile.Update as CPU
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame.Application.Effectful as FAE
import Flame.Subscription as FS
import Flame.Subscription.Unsafe.CustomEvent as FSUC
import Shared.Options.MountPoint (imID, profileID)
import Shared.Profile.View as SPV
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = do
      FAE.resumeMount (QuerySelector "#profile-edition-form") profileID {
            view: SPV.view,
            subscribe: [FS.onCustomEvent setChatExperiment SetProfileChatExperiment],
            init: Nothing,
            update: CPU.update
      }
      --a pain, but a chat experiment might be going on before loading the modal
      FSUC.broadcast' askChatExperiment
      --avatar changes
      input <- CPU.getFileInput
      CCF.setUpFileChange SetAvatar input profileID
