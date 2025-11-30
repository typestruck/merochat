module Server.Asks.Database where

import Droplet.Language
import Prelude hiding (not)

import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Server.Database as SD
import Server.Database.Asks (_answer, _answerer, _asker, asks)
import Server.Database.Blocks (_blocked, _blocker, blocks)
import Server.Database.DoppelgangerQuestions (_question)
import Server.Database.Fields (_id, _recipient, _sender, l, s, u)
import Server.Database.Histories (histories)
import Server.Database.Types (Checked(..))
import Server.Database.Users (_asksVisibility, _temporary, users)
import Server.Effect (ServerEffect)
import Shared.User (ProfileVisibility(..))

isAllowedToAsk :: Int -> Int -> ServerEffect Boolean
isAllowedToAsk loggedUserId userId = do
    found <- SD.single $ select _id # from (users # as u) # wher ( u ... _id .=. userId .&&.
              ( _asksVisibility .=. Everyone
                      .||. _asksVisibility
                      .=. NoTemporaryUsers
                      .&&. not (exists $ select (1 # as l) # from (users # as s) # wher (s ... _id .=. loggedUserId .&&. _temporary .=. Checked true))
                      .||. _asksVisibility
                      .=. Contacts
                      .&&. (exists $ select (1 # as l) # from (histories # as s) # wher (_sender .=. loggedUserId .&&. _recipient .=. userId .||. _recipient .=. loggedUserId .&&. _sender .=. userId))
              ) .&&. not (exists $ select (1 # as u) # from blocks # wher (_blocker .=. loggedUserId .&&. _blocked .=. u ... _id .||. _blocker .=. u ... _id .&&. _blocked .=. loggedUserId))
                .&&. not (exists $ select (1 # as u) # from asks # wher (_answerer .=. (u ... _id) .&&. _asker .=. loggedUserId .&&. isNull _answer))
      )
    pure $ DM.isJust found

saveAsk :: Int -> Int -> String -> ServerEffect Unit
saveAsk loggedUserId useId question = SD.execute $ insert # into asks (_asker /\ _answerer /\ _question) # values (loggedUserId /\ useId /\ question)