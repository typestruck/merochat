module Server.Leaderboard.Template where

import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.Leaderboard.Types (ToggleBoard(..))
import Shared.Leaderboard.View as SLV
import Server.Leaderboard.Types (Payload)

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
