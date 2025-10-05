module Server.Experiments.Template where

import Shared.Experiments.Types

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame as F
import Shared.Experiments.View as SEV
import Web.DOM.ParentNode (QuerySelector(..))

template ∷ _ → Effect String
template payload = F.preMount (QuerySelector "#chat-experiments")
      { view: SEV.view
      , model:
              { experiments: payload.experiments
              , visible: true
              , user: payload.user
              , section: HideExperiments
              , doppelganger:
                      { questions: []
                      , matches: []
                      , completed: payload.completedDoppelganger
                      , loading: false
                      , selectedChoice: Nothing
                      }
              }
      }
