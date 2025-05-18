module Server.Im.Database.Permission where

import Droplet.Language
import Prelude hiding (join)
import Server.Database.KarmaLeaderboard
import Server.Database.LastSeen
import Server.Database.Privileges

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Droplet.Driver (Pool)
import Server.Database as SD
import Server.Database.Fields
import Server.Database.Messages
import Server.Effect (BaseEffect)
import Shared.Privilege (Privilege(..))

canEditMessage ∷ ∀ r. Int → Int → BaseEffect { pool ∷ Pool | r } Boolean
canEditMessage loggedUserId messageId =
      map DM.isJust <<< SD.single $
            select (1 # as c)
                  # from messages
                  # wher (_id .=. messageId .&&. _sender .=. loggedUserId)

queryLastSeen ∷ NonEmptyArray Int → _
queryLastSeen ids = SD.query $ select (_who /\ _date) # from last_seen # wher (_who `in_` ids)

markdownPrivileges ∷ ∀ r. Int → BaseEffect { pool ∷ Pool | r } _
markdownPrivileges loggedUserId = SD.query $ select _feature # from (join privileges karma_leaderboard # on ((_feature .=. SendLinks .||. _feature .=. SendImages .||. _feature .=. SendAudios) .&&. _quantity .<=. _current_karma .&&. _ranker .=. loggedUserId))
