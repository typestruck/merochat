module Server.Feedback.Dabatase where

import Droplet.Language
import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Server.Database as SD
import Server.Database.Feedbacks
import Server.Types (ServerEffect)

insertFeedback ∷ Int → String → Maybe String → ServerEffect Unit
insertFeedback loggedUserId comments fileName = SD.execute $ insert # into feedbacks (_feedbacker /\ _comments /\ _fileName) # values (loggedUserId /\ comments /\ fileName)