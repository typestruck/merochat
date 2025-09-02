module Server.Experiments.Template where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Web.DOM.ParentNode (QuerySelector(..))
import Shared.Experiments.Types
import Flame as F
import Shared.Experiments.View as SEV

template ∷ _ → Effect String
template payload = F.preMount (QuerySelector "#chat-experiments")
      { view: SEV.view
      , model:
              { experiments: payload.experiments
              , current: Nothing
              , visible: true
              , confirming: Nothing
              , user: payload.user
              , section: HideSections
              }
      }
