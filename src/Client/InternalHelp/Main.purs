module Client.InternalHelp.Main where

import Prelude

import Client.InternalHelp.Update as CIHU
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Web.DOM.ParentNode (QuerySelector(..))
import Flame as F
import Flame.Subscription as FS
import Client.EventTypes (modalVisible)
import Shared.InternalHelp.Types (InternalHelpMessage(..))
import Shared.InternalHelp.View as SIHV

main ∷ Effect Unit
main =
      void $ F.resumeMount_ (QuerySelector "#internal-help")
            { view: SIHV.view
            , subscribe: [ FS.onCustomEvent' modalVisible (pure <<< Just <<< ToggleVisibility) ]
            , update: CIHU.update
            }