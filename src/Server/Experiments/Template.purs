module Server.Experiments.Template where

import Prelude
import Shared.Experiments.Types

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame as F
import Shared.Experiments.View as SEV
import Shared.Html (Html(..))
import Web.DOM.ParentNode (QuerySelector(..))

template ∷ _ → Effect Html
template payload = Html <$> F.preMount (QuerySelector "#chat-experiments")
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
              , paperPlane:
                      { loading: false
                      , message: Nothing
                      , thrown: payload.thrown
                      }
              }
      }
