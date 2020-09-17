module  Server.Leaderboard.Handler where

import Prelude
import Server.Types
import Shared.Types

import Data.Array as DA
import Data.Newtype as DN
import Run as R
--import Server.Leaderboard.Database as SLD
import Server.Leaderboard.Template as SLT

leaderboard :: { guards :: { loggedUserID :: PrimaryKey } } -> ServerEffect String
leaderboard { guards: { loggedUserID } } = do
      R.liftEffect $ SLT.template

