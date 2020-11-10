module Client.Profile.Main where

import Prelude

import Client.Common.File as CCF
import Client.Profile.Update as CPU
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame.Application.Effectful as FAE
import Shared.Profile.View as SPV
import Shared.Types
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = do
      channel <- FAE.resumeMount (QuerySelector "#profile-edition-form") {
            view: SPV.view,
            init: Nothing,
            update: CPU.update
      }

      input <- CPU.getFileInput
      CCF.setUpFileChange (Just <<< SetAvatar) input channel
