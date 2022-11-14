module Server.Experiments.Action where

import Prelude

import Server.Experiments.Database as SED
import Server.Effect (ServerEffect)

experiments ∷ Int -> ServerEffect _
experiments loggedUserId = do
      list ← SED.fetchExperiments
      user ← SED.fetchExperimentUser loggedUserId
      pure { experiments: list, user }