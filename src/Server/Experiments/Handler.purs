module Server.Experiments.Handler where

import Prelude
import Server.Types

import Run as R
import Server.Experiments.Template as SET
import Server.Experiments.Action as SEA

experiments ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect String
experiments _ = do
      listing <- SEA.experiments
      R.liftEffect $ SET.template listing
