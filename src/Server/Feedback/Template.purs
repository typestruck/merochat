module Server.Feedback.Template where

import Prelude
import Server.Effect

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Web.DOM.ParentNode (QuerySelector(..))
import Flame as F
import Shared.Element (ElementId(..))
import Shared.Feedback.View as SFV

template ∷ Effect String
template =
      F.preMount (QuerySelector $ "#" <> show FeedbackForm)
            { view: SFV.view
            , model:
                    { feedbackStatus: Nothing
                    , screenshot: Nothing
                    , visible: true
                    , comments: ""
                    , loading: false
                    }
            }
