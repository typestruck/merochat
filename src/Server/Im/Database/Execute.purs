module Server.Im.Database.Execute where

import Droplet.Language
import Prelude hiding (not)
import Server.Database.Blocks
import Server.Database.Fields
import Server.Database.Histories
import Server.Database.KarmaHistories
import Server.Database.Messages
import Server.Database.Reports
import Server.Database.Types
import Server.Database.Users
import Shared.Im.Types
import Shared.User

import Data.Array.NonEmpty as DAN
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Droplet.Driver (Pool)
import Server.Database as SD
import Server.Database.Functions (insert_history)
import Server.Effect (BaseEffect, ServerEffect)
import Server.Im.Database.Present (senderRecipientFilter)
import Shared.Unsafe as SU

isRecipientVisible ∷ ∀ r. Int → Int → BaseEffect { pool ∷ Pool | r } Boolean
isRecipientVisible loggedUserId userId =
      map DM.isJust <<< SD.single $
            select (1 # as c)
                  # from (leftJoin (users # as u) (histories # as h) # on (_sender .=. loggedUserId .&&. _recipient .=. userId .||. _sender .=. userId .&&. _recipient .=. loggedUserId))
                  # wher
                          ( u ... _id .=. userId
                                  .&&. not (exists $ select (1 # as c) # from blocks # wher (_blocked .=. loggedUserId .&&. _blocker .=. userId))
                                  .&&.
                                        (u ... _visibility .=. Everyone .||. u ... _visibility .=. NoTemporaryUsers .&&. exists (select (3 # as c) # from users # wher (_id .=. loggedUserId .&&. _temporary .=. Checked false)) .||. u ... _visibility .=. Contacts .&&. (isNotNull _first_message_date .&&. _visibility_last_updated .>=. _first_message_date))
                          )

deleteMessage ∷ ∀ r. Int → Int → Int → BaseEffect { pool ∷ Pool | r } Unit
deleteMessage loggedUserId userId messageId = SD.execute $
      delete
            # from messages
            # wher (_id .=. messageId .&&. _sender .=. loggedUserId .&&. _recipient .=. userId)

insertMessage ∷ ∀ r. Int → Int → String → BaseEffect { pool ∷ Pool | r } Int
insertMessage loggedUserId recipient content = SD.withTransaction $ \connection → do
      void $ SD.singleWith connection $ select (insert_history (loggedUserId /\ recipient) # as u)
      _.id <<< SU.fromJust <$> (SD.singleWith connection $ insert # into messages (_sender /\ _recipient /\ _content) # values (loggedUserId /\ recipient /\ content) # returning _id)

updateMessage ∷ ∀ r. Int → String → BaseEffect { pool ∷ Pool | r } Unit
updateMessage messageId content = void <<< SD.execute $ update messages # set ((_content .=. content) /\ (_edited .=. Checked true)) # wher (_id .=. messageId)

insertKarma ∷ ∀ r. Int → Int → Tuple Int Int → BaseEffect { pool ∷ Pool | r } Unit
insertKarma loggedUserId userId (Tuple senderKarma recipientKarma)
      | senderKarma <= 0 && recipientKarma <= 0 = pure unit
      | otherwise =
              SD.withTransaction $ \connection → do
                    when (senderKarma > 0) (SD.executeWith connection $ insert # into karma_histories (_amount /\ _target) # values (senderKarma /\ loggedUserId))
                    when (recipientKarma > 0) (SD.executeWith connection $ insert # into karma_histories (_amount /\ _target) # values (recipientKarma /\ userId))

changeStatus ∷ ∀ r. Int → MessageStatus → Array Int → BaseEffect { pool ∷ Pool | r } Unit
changeStatus loggedUserId status = case _ of
      [] → pure unit
      ids → SD.execute $ update messages # set (_status .=. status) # wher (_recipient .=. loggedUserId .&&. (_id `in_` (SU.fromJust $ DAN.fromArray ids)))

insertBlock ∷ Int → Int → ServerEffect Unit
insertBlock loggedUserId blocked = SD.execute $ blockQuery loggedUserId blocked

markAsDeleted ∷ Boolean → Int → { userId ∷ Int, messageId ∷ Int } → _
markAsDeleted isSender loggedUserId { userId, messageId }
      | isSender = SD.execute $ update histories # set (_sender_deleted_to .=. Just messageId) # senderRecipientFilter loggedUserId userId
      | otherwise = SD.execute $ update histories # set (_recipient_deleted_to .=. Just messageId) # senderRecipientFilter loggedUserId userId

blockQuery ∷ Int → Int → _
blockQuery blocker blocked = insert # into blocks (_blocker /\ _blocked) # values (blocker /\ blocked)

insertReport ∷ Int → Report → ServerEffect Unit
insertReport loggedUserId { userId, comment, reason } = SD.withTransaction $ \connection → do
      SD.executeWith connection $ blockQuery loggedUserId userId
      SD.executeWith connection $ insert # into reports (_reporter /\ _reported /\ _reason /\ _comment) # values (loggedUserId /\ userId /\ reason /\ comment)

updateTutorialCompleted ∷ Int → ServerEffect Unit
updateTutorialCompleted loggedUserId = SD.execute $ update users # set (_completedTutorial .=. Checked true) # wher (_id .=. loggedUserId)

chatHistoryEntry ∷ Int → Int → _
chatHistoryEntry loggedUserId otherId = SD.single $ select (_sender /\ _recipient) # from histories # senderRecipientFilter loggedUserId otherId

registerUser ∷ Int → String → String → ServerEffect Unit
registerUser loggedUserId email password = SD.execute $ update users # set ((_email .=. Just email) /\ (_password .=. Just password) /\ (_temporary .=. Checked false)) # wher (_id .=. loggedUserId)

upsertLastSeen ∷ ∀ r. String → BaseEffect { pool ∷ Pool | r } Unit
upsertLastSeen jsonInput = void $ SD.unsafeExecute "INSERT INTO last_seen(who, date) (SELECT * FROM jsonb_to_recordset(@jsonInput::jsonb) AS y (who integer, date timestamptz)) ON CONFLICT (who) DO UPDATE SET date = excluded.date" { jsonInput }

