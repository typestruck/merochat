module Server.Experiments.Database where

import Shared.Types


import Server.Database as SD
import Server.Types (ServerEffect)

--could also order by popularity
fecthExperiments :: ServerEffect (Array ChatExperiment)
fecthExperiments = SD.unsafeQuery "select id, code, name, description from experiments order by added desc" {}
