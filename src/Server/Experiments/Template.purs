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
              , current: Nothing
              , doppelganger:
                      { questions: []
                      , matches: []
                      , completed: payload.completedDoppelganger
                      , loading: false
                      , section: ShowDoppelganger
                      , selectedChoice: Nothing
                      }
              , debate:
                      { section: ShowNewDebate
                      , topic: Nothing
                      , statement: Nothing
                      , loading: false
                      , mine: []
                      , inFavor: Nothing
                      }

              , paperPlane:
                      { loading: false
                      , section: ShowNew
                      , message: Nothing
                      , thrown: payload.thrown
                      , flyingBy: payload.flyingBy
                      , caught: payload.caught
                      }
              }
      }
