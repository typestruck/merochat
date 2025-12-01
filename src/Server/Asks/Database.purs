module Server.Asks.Database where

import Droplet.Language
import Prelude hiding (not, join)

import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Droplet.Driver.Internal.Query (Connection(..))
import Server.Database as SD
import Server.Database.Asks (_answer, _answerer, _asker, asks)
import Server.Database.Blocks (_blocked, _blocker, blocks)
import Server.Database.Changelogs (_action, _changed, changelogs)
import Server.Database.DoppelgangerQuestions (_question)
import Server.Database.Experiments (_description)
import Server.Database.Fields (_date, _id, _name, _recipient, _sender, a, l, s, u)
import Server.Database.Histories (histories)
import Server.Database.Types (Checked(..))
import Server.Database.Users (_asksVisibility, _temporary, users)
import Server.Effect (ServerEffect)
import Shared.Ask (Ask)
import Shared.Changelog (ChangelogAction(..))
import Shared.User (ProfileVisibility(..))

presentAllAsks :: Int -> ServerEffect (Array Ask)
presentAllAsks loggedUserId = SD.query $ select ((a ... _id # as _id) /\ _asker /\ _answer /\ _name /\ _question) # from (join (asks # as a) (users # as u) # on (_asker .=. u ... _id)) # wher (_answerer .=. loggedUserId) # orderBy ((_date # desc))

isAllowedToAsk ∷ Int → Int → ServerEffect Boolean
isAllowedToAsk loggedUserId userId = do
      found ← SD.single $ select _id # from (users # as u) # wher
            ( u ... _id .=. userId
                    .&&.
                          ( _asksVisibility .=. Everyone
                                  .||. _asksVisibility
                                  .=. NoTemporaryUsers
                                  .&&. not (exists $ select (1 # as l) # from (users # as s) # wher (s ... _id .=. loggedUserId .&&. _temporary .=. Checked true))
                                  .||. _asksVisibility
                                  .=. Contacts
                                  .&&. (exists $ select (1 # as l) # from (histories # as s) # wher (_sender .=. loggedUserId .&&. _recipient .=. userId .||. _recipient .=. loggedUserId .&&. _sender .=. userId))
                          )
                    .&&. not (exists $ select (1 # as u) # from blocks # wher (_blocker .=. loggedUserId .&&. _blocked .=. u ... _id .||. _blocker .=. u ... _id .&&. _blocked .=. loggedUserId))
                    .&&. not (exists $ select (1 # as u) # from asks # wher (_answerer .=. (u ... _id) .&&. _asker .=. loggedUserId .&&. isNull _answer))
            )
      pure $ DM.isJust found

saveAsk ∷ Connection → Int → Int → String → _ Unit
saveAsk connection loggedUserId useId question = SD.executeWith connection $ insert # into asks (_asker /\ _answerer /\ _question) # values (loggedUserId /\ useId /\ question)

notifyAsk ∷ Connection → Int → _
notifyAsk connection userId = SD.executeWith connection $ insert # into changelogs (_changed /\ _description /\ _action) # values (Just userId /\ "You have a new ask! Answer here" /\ Just OpenProfilePage)