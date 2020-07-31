module Client.Profile.Main where

import Prelude

import Client.Common.DOM as CCD
import Client.Common.File as CCF
import Client.Common.Notification as CCN
import Client.Profile.Update as CPU
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2)
import Effect.Uncurried as EU
import Flame.Application.Effectful as FAE
import Shared.DateTime as SDT
import Shared.Profile.Types (Editors, ProfileMessage(..))
import Shared.Profile.View as SPV
import Shared.Types (Editor)
import Shared.Unsafe as SU
import Signal.Channel (Channel)
import Signal.Channel as SC
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.EventTarget as WET
import Web.HTML.HTMLInputElement as WHI

--loads the simplemde editors that are used for in place editing because content editable sucks balls
-- the editor for name and headline does not accept new lines
foreign import loadEditors :: Effect (Editors Editor Editor Editor)
foreign import blur_ :: EffectFn2 Editor (EffectFn1 String Unit) Unit

main :: Effect Unit
main = do
      minimumYear <- SDT.getMinimumYear
      channel <- FAE.resumeMount (QuerySelector ".profile-info-edition") {
            view: SPV.view minimumYear,
            init: Nothing,
            update: CPU.update
      }

      editors <- loadEditors
      EU.runEffectFn2 blur_ editors.name $ EU.mkEffectFn1 (SC.send channel <<< Just <<< SetName)
      EU.runEffectFn2 blur_ editors.headline $ EU.mkEffectFn1 (SC.send channel <<< Just <<< SetHeadline)
      EU.runEffectFn2 blur_ editors.description $ EU.mkEffectFn1 (SC.send channel <<< Just <<< SetDescription)
      SC.send channel <<< Just $ SetEditors editors

      input <- CPU.getFileInput
      CCF.setUpFileChange (Just <<< SetAvatar) input channel
