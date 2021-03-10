module Server.Experiments.Database where

import Shared.Types

import Database.PostgreSQL (Query(..), Row0(..))
import Server.Database as SD
import Server.Types (ServerEffect)

--could also order by popularity
fecthExperiments :: ServerEffect (Array ChatExperimentWrapper)
fecthExperiments = SD.select (Query "select id, code, name, description from experiments order by added desc") Row0
