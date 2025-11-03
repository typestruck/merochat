module Server.InternalHelp.Template where

import Prelude
import Server.Effect

import Effect (Effect)
import Flame as F
import Shared.Html (Html(..))
import Shared.InternalHelp.Types (DisplayHelpSection(..))
import Shared.InternalHelp.View as SIHV
import Web.DOM.ParentNode (QuerySelector(..))

template ∷ Effect Html
template =
      Html <$> F.preMount (QuerySelector "#internal-help")
            { view: SIHV.view
            , model:
                    { toggleHelp: FAQ
                    , visible: true
                    }
            }
