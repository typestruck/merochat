module Server.Feedback.Template where

import Prelude
import Server.Effect

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame as F
import Shared.Element (ElementId(..))
import Shared.Element as SE
import Shared.Feedback.View as SFV
import Shared.Html (Html(..))

template ∷ Effect Html
template =
      Html <$> F.preMount (SE.toQuerySelector FeedbackForm)
            { view: SFV.view
            , model:
                    { feedbackStatus: Nothing
                    , screenshot: Nothing
                    , visible: true
                    , comments: ""
                    , loading: false
                    }
            }
