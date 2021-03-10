module Client.Profile.Main where

import Prelude
import Shared.Types

import Client.Common.DOM (askChatExperiment, setChatExperiment)
import Client.Common.DOM as CCD
import Client.Common.File as CCF
import Client.Profile.Update as CPU
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame.Application.Effectful as FAE
import Shared.Profile.View as SPV
import Signal.Channel as SC
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = do
      channel <- FAE.resumeMount (QuerySelector "#profile-edition-form") {
            view: SPV.view,
            init: Nothing,
            update: CPU.update
      }
      --chat experiment changes
      CCD.addCustomEventListener setChatExperiment (SC.send channel <<< Just <<< SetProfileChatExperiment)
      --a pain, but a chat experiment might be going on before loading the modal
      CCD.dispatchCustomEvent $ CCD.createCustomEvent askChatExperiment unit
      --avatar changes
      input <- CPU.getFileInput
      CCF.setUpFileChange (Just <<< SetAvatar) input channel
