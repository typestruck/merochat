module Server.Experiments.Handler where

import Prelude
import Server.Types
import Shared.Types

import Run as R
import Server.Experiments.Template as SET
import Server.Experiments.Database as SED

experiments ∷ { guards ∷ { loggedUserID ∷ Int } } → ServerEffect String
experiments _ = do
      exp ← SED.fecthExperiments
      R.liftEffect $ SET.template exp
