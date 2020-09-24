module Client.Profile.Main where

import Prelude

import Client.Common.File as CCF
import Client.Profile.Update as CPU
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame.Application.Effectful as FAE
import Shared.DateTime as SDT
import Shared.Profile.View as SPV
import Shared.Types
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = do
      minimumYear <- SDT.getEarliestYear
      channel <- FAE.resumeMount (QuerySelector ".profile-edition") {
            view: SPV.view minimumYear,
            init: Nothing,
            update: CPU.update
      }

      input <- CPU.getFileInput
      CCF.setUpFileChange (Just <<< SetAvatar) input channel
