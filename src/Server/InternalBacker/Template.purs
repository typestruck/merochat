module Server.InternalBacker.Template where

import Prelude
import Server.Effect

import Effect (Effect)
import Flame as F
import Shared.Backer.View as SIBV
import Shared.Html (Html(..))
import Web.DOM.ParentNode (QuerySelector(..))

template ∷ Effect Html
template = Html <$> F.preMount (QuerySelector "#backer")
      { view: SIBV.view
      , model:
              { visible: true
              }
      }
