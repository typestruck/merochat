module Server.Experiments.Action where

import Prelude

import Server.Experiments.Database as SED
import Server.Types (ServerEffect)
import Shared.Experiments.Types (ChatExperiment)

experiments :: ServerEffect (Array ChatExperiment)
experiments = SED.fetchExperiments