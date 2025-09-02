module Client.Profile.Main where

import Prelude

import Client.Common.File as CCF
import Client.Profile.Update as CPU
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect (Effect)
import Flame as F
import Flame.Subscription as FS
import Shared.Element (ElementId(..))
import Shared.Im.EventTypes (modalVisible)
import Shared.Options.MountPoint (profileId)
import Shared.Profile.Types (ProfileMessage(..))
import Shared.Profile.View as SPV
import Web.DOM.ParentNode (QuerySelector(..))

main ∷ Effect Unit
main = do
      void $ F.resumeMount (QuerySelector $ "#" <> show ProfileEditionForm) profileId
            { view: SPV.view
            , subscribe: [ FS.onCustomEvent modalVisible ToggleVisibility ]
            , update: CPU.update
            }
      --avatar changes
      input ← CPU.getFileInput
      CCF.setUpFileChange (\_ _ b → SetPField $ _ { avatarInputed = Just b }) input profileId
