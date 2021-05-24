module Server.Experiments.Database where

import Shared.Types

import Shared.Experiments.Types
import Server.Database as SD
import Server.Types (ServerEffect)

--could also order by popularity
fecthExperiments :: ServerEffect (Array ChatExperiment)
fecthExperiments = SD.query $ select (_id /\ _code /\ _name /\ _description) # from experiments # orderBy (_added # desc)
