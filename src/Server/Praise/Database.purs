module Server.Praise.Database where

import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Droplet.Driver.Internal.Query (Connection)
import Droplet.Language (as, exists, from, insert, into, not, select, values, wher, (.&&.), (...), (.=.), (.||.))
import Prelude (Unit, bind, map, pure, (#), ($))
import Server.Database as SD
import Server.Database.Blocks (_blocked, _blocker, blocks)
import Server.Database.Changelogs (_action, _changed, changelogs)
import Server.Database.Experiments (_description)
import Server.Database.Fields (_id, u)
import Server.Database.Praise (_accepted, _praised, _praisedFor, _praiser, praises)
import Server.Database.Users (users)
import Server.Effect (ServerEffect)
import Shared.Changelog (ChangelogAction(..))
import Shared.Praise (PraisedFor)

isAllowedToPraise ∷ Int → Int → ServerEffect Boolean
isAllowedToPraise loggedUserId userId = do
      found ← SD.single $ select _id # from (users # as u) # wher
            ( u ... _id .=. userId
                    .&&. not (exists $ select (1 # as u) # from blocks # wher (_blocker .=. loggedUserId .&&. _blocked .=. u ... _id .||. _blocker .=. u ... _id .&&. _blocked .=. loggedUserId))
                    .&&. not (exists $ select (1 # as u) # from praises # wher (_praised .=. (u ... _id) .&&. _praiser .=. loggedUserId))
            )
      pure $ DM.isJust found

savePraise ∷ Connection → Int → Int → Array PraisedFor → _ Unit
savePraise connection loggedUserId userId for = SD.executeWith connection $ insert # into praises (_praiser /\ _praised /\ _praisedFor /\ _accepted) # values (map row for)
      where
      row p = loggedUserId /\ userId /\ p /\ true

notifyPraise ∷ Connection → Int → _ Unit
notifyPraise connection userId = SD.executeWith connection $ insert # into changelogs (_changed /\ _description /\ _action) # values (Just userId /\ "You have a new praise! See here" /\ Just OpenProfilePage)