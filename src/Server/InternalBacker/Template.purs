module Server.InternalBacker.Template where

import Server.Effect

import Effect (Effect)
import Web.DOM.ParentNode (QuerySelector(..))
import Flame as F
import Shared.Backer.View as SIBV

template ∷ Effect String
template = F.preMount (QuerySelector "#backer")
      { view: SIBV.view
      , model:
              { visible: true
              }
      }
