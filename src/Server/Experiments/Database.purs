module Server.Experiments.Database where

import Droplet.Language
import Prelude hiding (join)
import Server.Database.Experiments
import Server.Database.Fields
import Server.Database.KarmaLeaderboard
import Server.Database.Privileges
import Shared.Experiments.Types

import Data.Tuple.Nested ((/\))
import Server.Database as SD
import Server.Effect (ServerEffect)
import Shared.Privilege (Privilege)
import Shared.Unsafe as SU

--could also order by popularity
fetchExperiments ∷ ServerEffect (Array ChatExperiment)
fetchExperiments = SD.query $ select (_id /\ _code /\ _name /\ _description) # from experiments # orderBy (_code # desc)

fetchExperimentUser ∷ Int → ServerEffect ChatExperimentUser
fetchExperimentUser loggedUserId = do
      record ← SD.single $ select (array_agg _feature # as _privileges) # from (join privileges karma_leaderboard # on (_ranker .=. loggedUserId .&&. _quantity .<=. _current_karma))
      pure <<< { privileges: _ } $ SU.fromJust do
            r ← record
            r.privileges
