module Client.Experiments.Main where

import Prelude

import Client.Experiments.Update as CEU
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Web.DOM.ParentNode (QuerySelector(..))
import Flame as F
import Flame.Subscription as FS
import Shared.Experiments.Types (ExperimentsMessage(..))
import Shared.Experiments.View as SEV
import Client.EventTypes (modalVisible)
import Client.AppId (experimentsAppId)

main ∷ Effect Unit
main =
      void $ F.resumeMount (QuerySelector "#chat-experiments") experimentsAppId
            { view: SEV.view
            , subscribe: [ FS.onCustomEvent' modalVisible (pure <<< Just <<< ToggleVisibility) ]
            , update: CEU.update
            }