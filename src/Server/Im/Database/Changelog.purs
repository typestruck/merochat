module Server.Im.Database.Changelog where

import Droplet.Language (insert, into, values)
import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Server.Database as SD
import Server.Database.ChangelogRead (_log, changelog_read)
import Server.Database.LastSeen (_who)
import Server.Effect (ServerEffect)
import Shared.Changelog (Changelog)

listChangelogs ∷ Int → Maybe Int → ServerEffect (Array Changelog)
listChangelogs loggedUserId id = do
      SD.unsafeQuery "SELECT c.id id, description, changed, (log IS NOT NULL) read FROM changelogs c LEFT JOIN changelog_read ON c.id = log AND who = @loggedUserId WHERE ((changed = @loggedUserId AND log IS NOT NULL OR changed IS NULL OR c.id < @id) AND date >= (SELECT joined FROM users WHERE id = @loggedUserId)) ORDER BY date DESC LIMIT 5" { loggedUserId, id }

markRead ∷ Int → Array Int → ServerEffect Unit
markRead loggedUserId ids = SD.execute $ insert # into changelog_read (_who /\ _log) # values ((loggedUserId /\ _) <$> ids)

