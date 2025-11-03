module Server.KarmaPrivileges.Template where

import Prelude

import Effect (Effect)
import Flame as F
import Shared.Html (Html(..))
import Shared.KarmaPrivileges.Types (ToggleBoard(..))
import Shared.KarmaPrivileges.View as SLV
import Web.DOM.ParentNode (QuerySelector(..))

template ∷ _ → Effect Html
template { top10, inBetween10, userPosition, privileges, stats } =
      Html <$> F.preMount (QuerySelector "#karma-leaderboard")
            { view: SLV.view
            , model:
                    { toggleBoard: InBetween10
                    , top10
                    , inBetween10
                    , userPosition
                    , privileges
                    , visible: true
                    , stats
                    }
            }
