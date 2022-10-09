module Server.KarmaPrivileges.Template where

import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.KarmaPrivileges.Types (ToggleBoard(..))
import Shared.KarmaPrivileges.View as SLV
import Server.KarmaPrivileges.Types (Payload)

template ∷ Payload → Effect String
template { top10, inBetween10, userPosition } =
      F.preMount (QuerySelector ".karma-leaderboard")
            { view: SLV.view
            , init:
                    { toggleBoard: InBetween10
                    , top10
                    , inBetween10
                    , userPosition
                    }
            }
