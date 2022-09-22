module Server.Feedback.Dabatase where

import Droplet.Language
import Prelude
import Server.Database.Feedbacks

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Server.Database as SD
import Server.Database.Fields (_id)
import Server.Types (ServerEffect)
import Shared.Unsafe as SU

insertFeedback ∷ Int → String → Maybe String → ServerEffect Int
insertFeedback loggedUserId comments fileName = map (_.id <<< SU.fromJust) <<< SD.single $ insert # into feedbacks (_feedbacker /\ _comments /\ _fileName) # values (loggedUserId /\ comments /\ fileName) # returning _id