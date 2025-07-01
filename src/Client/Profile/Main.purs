module Client.Profile.Main where

import Prelude
import Shared.Profile.Types (Field(..), ProfileMessage(..))

import Client.Common.File as CCF
import Client.Profile.Update as CPU
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame.Application.Effectful as FAE
import Flame.Subscription as FS
import Shared.Element (ElementId(..))
import Shared.Im.EventTypes (modalVisible)
import Shared.Options.MountPoint (profileId)
import Shared.Profile.View as SPV
import Web.DOM.ParentNode (QuerySelector(..))

main ∷ Effect Unit
main = do
      FAE.resumeMount (QuerySelector $ "#" <> show ProfileEditionForm) profileId
            { view: SPV.view
            , subscribe: [ FS.onCustomEvent modalVisible ToggleVisibility ]
            , init: Nothing
            , update: CPU.update
            }
      --avatar changes
      input ← CPU.getFileInput
      CCF.setUpFileChange (\_ _ b → Save <<< Avatar $ Just b) input profileId
