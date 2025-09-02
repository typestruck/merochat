module Server.KarmaPrivileges.Template where

import Effect (Effect)
import Web.DOM.ParentNode (QuerySelector(..))
import Flame as F
import Shared.KarmaPrivileges.Types (ToggleBoard(..))
import Shared.KarmaPrivileges.View as SLV

template ∷ _ → Effect String
template { top10, inBetween10, userPosition, privileges, stats } =
      F.preMount (QuerySelector "#karma-leaderboard")
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
