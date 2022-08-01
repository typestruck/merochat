module Server.IM.Database where

import Droplet.Language
import Prelude hiding (join, not)
import Server.Database.Blocks
import Server.Database.Countries
import Server.Database.Fields
import Server.Database.Functions
import Server.Database.Histories
import Server.Database.Histories
import Server.Database.KarmaHistories
import Server.Database.KarmaLeaderboard
import Server.Database.Languages
import Server.Database.LanguagesUsers
import Server.Database.Messages
import Server.Database.Reports
import Server.Database.Suggestions
import Server.Database.Tags
import Server.Database.TagsUsers
import Server.Database.Users
import Server.Types
import Shared.ContentType
import Shared.IM.Types

import Control.Monad.List.Trans (filter)
import Data.Array as DA
import Data.BigInt as DB
import Data.DateTime (DateTime(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String.Common as DS
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Server.Database.LastSeen
import Debug (spy)
import Droplet.Driver (Pool)
import Droplet.Language.Internal.Condition (class ToCondition, Exists, Not)
import Droplet.Language.Internal.Definition (Path)
import Droplet.Language.Internal.Function (PgFunction)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Server.Database as SD
import Server.IM.Database.Flat (FlatContactHistoryMessage, FlatUser, FlatContact)
import Shared.Options.Page (contactsPerPage, initialMessagesPerPage, messagesPerPage, suggestionsPerPage)
import Shared.Unsafe as SU
import Shared.User (ProfileVisibility(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

userPresentationFields =
      (u ... _id # as _id)
            /\ _avatar
            /\ _gender
            /\ (date_part_age ("year" /\ _birthday) # as _age)
            /\ _name
            /\ (_visibility # as profileVisibility)
            /\ (_readReceipts # as readReceipts)
            /\ (_typingStatus # as typingStatus)
            /\ (_onlineStatus # as onlineStatus)
            /\ (_messageTimestamps # as messageTimestamps)
            /\ _headline
            /\ _description
            /\ (select _name # from countries # wher (_id .=. u ... _country) # orderBy _id # limit (Proxy :: _ 1) # as _country)
            /\ (select (string_agg (l ... _name) (", " # orderBy _name) # as _languages) # from (((languages # as l) `join` (languages_users # as lu)) # on (l ... _id .=. lu ... _language .&&. lu ... _speaker .=. u ... _id)) # orderBy _languages # limit (Proxy :: _ 1))
            /\ (select (string_agg _name ("\n" # orderBy (l ... _id)) # as _tags) # from (((tags # as l) `join` (tags_users # as tu)) # on (l ... _id .=. tu ... _tag .&&. tu ... _creator .=. u ... _id)) # orderBy _tags # limit (Proxy :: _ 1))
            /\ (k ... _current_karma # as _karma)
            /\ (_position # as _karmaPosition)

contactPresentationFields uid = distinct $ (coalesce (_sender /\ uid) # as _chatStarter) /\ (h ... _date # as _lastMessageDate) /\ (datetime_part_age ("day" /\ coalesce (_first_message_date /\ utc_now)) # as _chatAge) /\ userPresentationFields

senderRecipientFilter loggedUserId otherId = wher ((_sender .=. loggedUserId .&&. _recipient .=. otherId) .||. (_sender .=. otherId .&&. _recipient .=. loggedUserId))

contactsSource ∷ Int → _
contactsSource loggedUserId = join usersSource (histories # as h) # on (u ... _id .=. h ... _sender .&&. h ... _recipient .=. loggedUserId .||. u ... _id .=. h ... _recipient .&&. h ... _sender .=. loggedUserId)

usersSource ∷ _
usersSource = join (users # as u) (karma_leaderboard # as k) # on (u ... _id .=. k ... _ranker)

presentUser ∷ Int → ServerEffect (Maybe FlatUser)
presentUser loggedUserId = SD.single $ select userPresentationFields # from usersSource # wher (u ... _id .=. loggedUserId .&&. _visibility .<>. TemporarilyBanned)

suggest ∷ Int → Int → Maybe ArrayPrimaryKey → ServerEffect (Array FlatUser)
suggest loggedUserId skip = case _ of
      Just (ArrayPrimaryKey []) → -- no users to avoid when impersonating

            SD.query $ suggestBaseQuery skip baseFilter
      Just (ArrayPrimaryKey keys) → -- users to avoid when impersonating

            SD.query $ suggestBaseQuery skip (baseFilter .&&. not (in_ (u ... _id) keys))
      _ → -- default case

            SD.query $ suggestBaseQuery skip baseFilter
      where
      baseFilter = (u ... _id .<>. loggedUserId .&&. _visibility .=. Everyone .&&. not (exists $ select (1 # as u) # from blocks # wher (_blocker .=. loggedUserId .&&. _blocked .=. u ... _id .||. _blocker .=. u ... _id .&&. _blocked .=. loggedUserId)))

-- top level to avoid monomorphic filter
suggestBaseQuery skip filter =
      select star
            # from
                  ( select userPresentationFields
                          # from (join usersSource (suggestions # as s) # on (u ... _id .=. _suggested))
                          # wher filter
                          # orderBy (s ... _id)
                          # limit (Proxy :: Proxy 10)
                          # offset skip
                          # as t
                  )
            # orderBy random

presentUserContactFields ∷ String
presentUserContactFields =
      """ h.sender "chatStarter"
      , h.recipient
      , h.sender_deleted_to
      , h.recipient_deleted_to
      , h.last_message_date "lastMessageDate"
      , date_part_age ('day', COALESCE(first_message_date, utc_now())) "chatAge"
      , u.id
      , avatar
      , gender
      , date_part_age ('year', birthday) age
      , name
      , visibility "profileVisibility"
      , read_receipts "readReceipts"
      , typing_status "typingStatus"
      , online_status "onlineStatus"
      , message_timestamps "messageTimestamps"
      , headline
      , description
      , (SELECT name FROM countries WHERE id = u.country) country
      , (SELECT STRING_AGG(l.name, ', ' ORDER BY name) FROM languages l JOIN languages_users lu ON l.id = lu.language AND lu.speaker = u.id) languages
      , (SELECT STRING_AGG(t.name, '\n' ORDER BY t.id) FROM tags t JOIN tags_users tu ON t.id = tu.tag AND tu.creator = u.id) tags
      , k.current_karma karma
      , position "karmaPosition"
"""

presentMessageContactFields ∷ String
presentMessageContactFields =
      """
      , s.id as "messageId"
      , s.sender
      , s.recipient
      , s.date
      , s.content
      , s.status """

presentContactFields ∷ String
presentContactFields = presentUserContactFields <> presentMessageContactFields

presentContacts ∷ Int → Int → ServerEffect (Array FlatContactHistoryMessage)
presentContacts loggedUserId skip = presentNContacts loggedUserId contactsPerPage skip

presentNContacts ∷ Int → Int → Int → ServerEffect (Array FlatContactHistoryMessage)
presentNContacts loggedUserId n skip = SD.unsafeQuery query
      { loggedUserId
      , status: Read
      , initialMessages: initialMessagesPerPage
      , contact: Contacts
      , everyone: Everyone
      , limit: n
      , offset: skip
      }
      where
      --refactor: paginate over deleted chats in a cleaner way
      query =
            "SELECT * FROM (SELECT" <> presentUserContactFields
                  <>
                        """FROM
      users u
      JOIN karma_leaderboard k ON u.id = k.ranker
      JOIN histories h ON u.id = sender AND recipient = @loggedUserId OR u.id = recipient AND sender = @loggedUserId
      WHERE (visibility = @contact OR visibility = @everyone)
            AND NOT EXISTS (SELECT 1 FROM blocks WHERE blocker = h.recipient AND blocked = h.sender OR blocker = h.sender AND blocked = h.recipient)
            AND (h.sender = @loggedUserId AND (h.sender_deleted_to IS NULL OR EXISTS(SELECT 1 FROM messages WHERE id > h.sender_deleted_to AND (sender = @loggedUserId AND recipient = h.recipient OR sender = h.recipient AND recipient = @loggedUserId))) OR h.recipient = @loggedUserId AND (h.recipient_deleted_to IS NULL OR EXISTS(SELECT 1 FROM messages WHERE id > h.recipient_deleted_to AND (recipient = @loggedUserId AND sender = h.sender OR recipient = h.sender AND recipient = @loggedUserId))))
      ORDER BY last_message_date DESC LIMIT @limit OFFSET @offset) uh
      , LATERAL (SELECT *
                 FROM (SELECT
                              ROW_NUMBER() OVER (ORDER BY date DESC) n"""
                  <> presentMessageContactFields
                  <>
                        """FROM messages s
                       WHERE (s.sender = uh."chatStarter" AND s.recipient = uh.recipient OR
                              s.sender = uh.recipient AND s.recipient = uh."chatStarter") AND
                              NOT (uh."chatStarter" = @loggedUserId AND uh.sender_deleted_to IS NOT NULL AND s.id <= uh.sender_deleted_to OR
                                   uh.recipient = @loggedUserId AND uh.recipient_deleted_to IS NOT NULL AND s.id <= uh.recipient_deleted_to)
                       ORDER BY date DESC) b
                 WHERE status < @status OR n <= @initialMessages
                 ORDER BY date) s"""

--refactor: this can use droplet
presentSingleContact ∷ Int → Int → Int → ServerEffect (Array FlatContactHistoryMessage)
presentSingleContact loggedUserId userId offset = SD.unsafeQuery query
      { loggedUserId
      , userId
      , contact: Contacts
      , everyone: Everyone
      , messagesPerPage
      , offset
      }
      where
      query = "SELECT * FROM (SELECT" <> presentContactFields <>
            """FROM users u
      JOIN karma_leaderboard k ON u.id = k.ranker
      JOIN histories h ON u.id = h.sender AND h.recipient = @loggedUserId OR u.id = h.recipient AND h.sender = @loggedUserId
      JOIN messages s ON s.sender = h.sender AND s.recipient = h.recipient OR s.sender = h.recipient AND s.recipient = h.sender
WHERE (visibility = @contact OR visibility = @everyone)
      AND u.id = @userId
      AND NOT (h.sender = @loggedUserId AND h.sender_deleted_to IS NOT NULL AND s.id <= h.sender_deleted_to OR
               h.recipient = @loggedUserId AND h.recipient_deleted_to IS NOT NULL AND s.id <= h.recipient_deleted_to)
      AND NOT EXISTS (SELECT 1 FROM blocks WHERE blocker = h.recipient AND blocked = h.sender OR blocker = h.sender AND blocked = h.recipient)
ORDER BY s.date DESC
LIMIT @messagesPerPage
OFFSET @offset) m ORDER BY m.date"""

--refactor: this can use droplet
presentMissedContacts ∷ Int → Int → ServerEffect (Array FlatContactHistoryMessage)
presentMissedContacts loggedUserId lastId = SD.unsafeQuery query
      { loggedUserId
      , status: Delivered
      , contact: Contacts
      , everyone: Everyone
      , lastId
      }
      where
      query = "SELECT" <> presentContactFields <>
            """FROM users u
      JOIN karma_leaderboard k ON u.id = k.ranker
      JOIN histories h ON u.id = h.sender AND h.recipient = @loggedUserId OR u.id = h.recipient AND h.sender = @loggedUserId
      JOIN messages s ON s.sender = h.sender OR s.sender = h.recipient
WHERE (visibility = @contact OR visibility = @everyone)
      AND NOT EXISTS (SELECT 1 FROM blocks WHERE blocker = h.recipient AND blocked = h.sender OR blocker = h.sender AND blocked = h.recipient)
      AND s.status < @status
      AND s.recipient = @loggedUserId
      AND s.id > @lastId
ORDER BY "lastMessageDate" DESC, s.sender, s.date"""

messageIdsFor ∷ Int → Int → ServerEffect (Array TemporaryMessageId)
messageIdsFor loggedUserId messageId = SD.query $ select (_id /\ (_temporary_id # as (Proxy ∷ _ "temporaryId"))) # from messages # wher (_sender .=. loggedUserId .&&. _id .>. messageId)

countChats ∷ Int → ServerEffect Int
countChats loggedUserId = map (DM.maybe 0 (SU.fromJust <<< DB.toInt <<< _.t)) $ SD.single $ select (count _id # as t) # from histories # wher (_sender .=. loggedUserId .||. _recipient .=. loggedUserId)

isRecipientVisible ∷ ∀ r. Int → Int → BaseEffect { pool ∷ Pool | r } Boolean
isRecipientVisible loggedUserId userId =
      map DM.isJust <<< SD.single $
            select (1 # as c)
                  # from (leftJoin (users # as u) (histories # as h) # on (_sender .=. loggedUserId .&&. _recipient .=. userId .||. _sender .=. userId .&&. _recipient .=. loggedUserId))
                  # wher (u ... _id .=. userId .&&. not (exists $ select (1 # as c) # from blocks # wher (_blocked .=. loggedUserId .&&. _blocker .=. userId)) .&&. (u ... _visibility .=. Everyone .||. u ... _visibility .=. Contacts .&&. (isNotNull _first_message_date .&&. _visibility_last_updated .>=. _first_message_date)))

insertMessage ∷ ∀ r. Int → Int → Int → String → BaseEffect { pool ∷ Pool | r } Int
insertMessage loggedUserId recipient temporaryId content = SD.withTransaction $ \connection → do
      void $ SD.singleWith connection $ select (insert_history (loggedUserId /\ recipient) # as u)
      _.id <<< SU.fromJust <$> (SD.singleWith connection $ insert # into messages (_sender /\ _recipient /\ _temporary_id /\ _content) # values (loggedUserId /\ recipient /\ temporaryId /\ content) # returning _id)

insertKarma ∷ ∀ r. Int → Int → Tuple Int Int → BaseEffect { pool ∷ Pool | r } Unit
insertKarma loggedUserId otherID (Tuple senderKarma recipientKarma) =
      void <<< SD.execute $ insert # into karma_histories (_amount /\ _target) # values
            [ senderKarma /\ loggedUserId
            , recipientKarma /\ otherID
            ]

changeStatus ∷ ∀ r. Int → MessageStatus → Array Int → BaseEffect { pool ∷ Pool | r } Unit
changeStatus loggedUserId status ids = SD.execute $ update messages # set (_status .=. status) # wher (_recipient .=. loggedUserId .&&. (_id `in_` ids))

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

chatHistoryEntry ∷ Int → Int → _
chatHistoryEntry loggedUserId otherId = SD.single $ select (_sender /\ _recipient) # from histories # senderRecipientFilter loggedUserId otherId

upsertLastSeen :: forall r. String -> BaseEffect { pool ∷ Pool | r } Unit
upsertLastSeen jsonInput = void $ SD.unsafeExecute "INSERT INTO last_seen(who, date) (SELECT * FROM jsonb_to_recordset(@jsonInput::jsonb) AS y (who integer, date timestamptz)) ON CONFLICT (who) DO UPDATE SET date = excluded.date" { jsonInput }

queryLastSeen :: Array Int -> _
queryLastSeen ids = SD.query $ select (_who /\ _date) # from last_seen # wher (_who `in_` ids)

_chatStarter ∷ Proxy "chatStarter"
_chatStarter = Proxy

_chatAge ∷ Proxy "chatAge"
_chatAge = Proxy

h ∷ Proxy "h"
h = Proxy

s ∷ Proxy "s"
s = Proxy

t ∷ Proxy "t"
t = Proxy

_lastMessageDate ∷ Proxy "lastMessageDate"
_lastMessageDate = Proxy