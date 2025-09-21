module Server.Im.Database.Present where

import Droplet.Language
import Prelude hiding (join, not)
import Server.Database.Badges
import Server.Database.BadgesUsers
import Server.Database.Countries
import Server.Database.Fields
import Server.Database.KarmaLeaderboard
import Server.Database.Languages
import Server.Database.LanguagesUsers
import Server.Database.LastSeen
import Server.Database.Privileges
import Server.Database.Suggestions
import Server.Database.Tags
import Server.Database.TagsUsers
import Server.Database.Users
import Server.Im.Database.Flat

import Data.DateTime (DateTime(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Server.Database as SD
import Server.Database.CompleteProfiles (_completed, _completer, complete_profiles)
import Server.Database.Functions (date_part_age)
import Server.Effect (ServerEffect)
import Shared.Im.Types (HistoryMessage, MessageStatus(..))
import Shared.Options.Page (contactsPerPage, initialMessagesPerPage, messagesPerPage)
import Shared.User (ProfileVisibility(..))
import Type.Proxy (Proxy(..))

userPresentationFields =
      userFields
      /\ (1 # as _bin)
      /\ (false # as _isContact)
      /\ completeness

completeness = (select (array_agg (_completed # orderBy  _completed) # as _completedFields) # from complete_profiles # wher (_completer .=. u ... _id) # orderBy _completedFields # limit (Proxy ∷ _ 1))
      where  _completedFields = Proxy :: Proxy "completedFields"

userFields =
      (u ... _id # as _id)
            /\ _avatar
            /\ _gender
            /\ (date_part_age ("year" /\ _birthday) # as _age)
            /\ _name
            /\ (_visibility # as profileVisibility)
            /\ (_readReceipts # as readReceipts)
            /\ (_typingStatus # as typingStatus)
            /\ _temporary
            /\ (_postsVisibility # as postsVisibility)
            /\ _backer
            /\ (_onlineStatus # as onlineStatus)
            /\ (_completedTutorial # as completedTutorial)
            /\ (l ... _date # as _lastSeen)
            /\ (_messageTimestamps # as messageTimestamps)
            /\ (select (array_agg (l ... _name # orderBy (l ... _name)) # as _languages) # from (((languages # as l) `join` (languages_users # as lu)) # on (l ... _id .=. lu ... _language .&&. lu ... _speaker .=. u ... _id)) # orderBy _languages # limit (Proxy ∷ _ 1))
            /\ _joined
            /\ _headline
            /\ _description
            /\ (select _name # from countries # wher (_id .=. u ... _country) # orderBy _id # limit (Proxy ∷ _ 1) # as _country)
            /\ (select (array_agg _feature # as _privileges) # from privileges # wher (_quantity .<=. k ... _current_karma) # orderBy _privileges # limit (Proxy ∷ _ 1))
            /\ (select (array_agg _description # as _badges) # from (((badges # as b) `join` (badges_users # as bu)) # on (b ... _id .=. bu ... _badge .&&. bu ... _receiver .=. u ... _id)) # orderBy _badges # limit (Proxy ∷ _ 1))
            /\ (select (array_agg (l ... _name # orderBy (l ... _id)) # as _tags) # from (((tags # as l) `join` (tags_users # as tu)) # on (l ... _id .=. tu ... _tag .&&. tu ... _creator .=. u ... _id)) # orderBy _tags # limit (Proxy ∷ _ 1))
            /\ (k ... _current_karma # as _karma)
            /\ (_position # as _karmaPosition)

senderRecipientFilter loggedUserId otherId = wher ((_sender .=. loggedUserId .&&. _recipient .=. otherId) .||. (_sender .=. otherId .&&. _recipient .=. loggedUserId))

usersSource ∷ _
usersSource = join (join (users # as u) (karma_leaderboard # as k) # on (u ... _id .=. k ... _ranker)) (last_seen # as l) # on (u ... _id .=. _who)

presentUserContactFields ∷ String
presentUserContactFields =
      """ h.sender "chatStarter"
      , h.recipient
      , h.sender_deleted_to
      , h.recipient_deleted_to
      , h.last_message_date "lastMessageDate"
      , date_part ('day', utc_now() - COALESCE(first_message_date, utc_now())) "chatAge"
      , u.posts_visibility as "postsVisibility"
      , false as "isContact"
      , u.id
      , avatar
      , gender
      , temporary
      , joined
      , backer
      , completed_tutorial "completedTutorial"
      , date_part_age ('year', birthday) age
      , name
      , 1 as bin
      , ls.date as "lastSeen"
      , visibility "profileVisibility"
      , read_receipts "readReceipts"
      , typing_status "typingStatus"
      , array[]::integer[] as "privileges"
      , array[]::integer[] as "completedFields"
      , online_status "onlineStatus"
      , message_timestamps "messageTimestamps"
      , headline
      , description
      , (SELECT name FROM countries WHERE id = u.country) country
      , (SELECT ARRAY_AGG(l.name ORDER BY name) FROM languages l JOIN languages_users lu ON l.id = lu.language AND lu.speaker = u.id) languages
      , (SELECT ARRAY_AGG(description) FROM badges b JOIN badges_users bu ON b.id = bu.badge AND bu.receiver = u.id) badges
      , (SELECT ARRAY_AGG(t.name ORDER BY t.id) FROM tags t JOIN tags_users tu ON t.id = tu.tag AND tu.creator = u.id) tags
      , k.current_karma karma
      , position "karmaPosition"
"""

presentMessageFields ∷ String
presentMessageFields =
      """
       s.id as "messageId"
      , s.sender
      , s.recipient
      , s.date
      , s.edited
      , s.content
      , s.status """

presentContactFields ∷ String
presentContactFields = presentUserContactFields <> "," <> presentMessageFields

presentContacts ∷ Int → Int → ServerEffect (Array FlatContactHistoryMessage)
presentContacts loggedUserId skip = presentNContacts loggedUserId contactsPerPage skip

presentNContacts ∷ Int → Int → Int → ServerEffect (Array FlatContactHistoryMessage)
presentNContacts loggedUserId n skip = SD.unsafeQuery query
      { loggedUserId
      , status: Read
      , initialMessages: initialMessagesPerPage
      , contacts: Contacts
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
      LEFT JOIN last_seen ls ON u.id = ls.who
      WHERE visibility <= @contacts
            AND NOT EXISTS (SELECT 1 FROM blocks WHERE blocker = h.recipient AND blocked = h.sender OR blocker = h.sender AND blocked = h.recipient)
            AND (h.sender = @loggedUserId AND (h.sender_deleted_to IS NULL OR EXISTS(SELECT 1 FROM messages WHERE id > h.sender_deleted_to AND (sender = @loggedUserId AND recipient = h.recipient OR sender = h.recipient AND recipient = @loggedUserId))) OR h.recipient = @loggedUserId AND (h.recipient_deleted_to IS NULL OR EXISTS(SELECT 1 FROM messages WHERE id > h.recipient_deleted_to AND (recipient = @loggedUserId AND sender = h.sender OR recipient = h.sender AND recipient = @loggedUserId))))
      ORDER BY last_message_date DESC LIMIT @limit OFFSET @offset) uh
      , LATERAL (SELECT *
                 FROM (SELECT
                              ROW_NUMBER() OVER (ORDER BY date DESC) n,"""
                  <> presentMessageFields
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
      , contacts: Contacts
      , messagesPerPage
      , offset
      }
      where
      query = "SELECT * FROM (SELECT" <> presentContactFields <>
            """FROM users u
      JOIN karma_leaderboard k ON u.id = k.ranker
      JOIN histories h ON u.id = h.sender AND h.recipient = @loggedUserId OR u.id = h.recipient AND h.sender = @loggedUserId
      JOIN messages s ON s.sender = h.sender AND s.recipient = h.recipient OR s.sender = h.recipient AND s.recipient = h.sender
      LEFT JOIN last_seen ls ON u.id = ls.who
WHERE visibility <= @contacts
      AND u.id = @userId
      AND NOT (h.sender = @loggedUserId AND h.sender_deleted_to IS NOT NULL AND s.id <= h.sender_deleted_to OR
               h.recipient = @loggedUserId AND h.recipient_deleted_to IS NOT NULL AND s.id <= h.recipient_deleted_to)
      AND NOT EXISTS (SELECT 1 FROM blocks WHERE blocker = h.recipient AND blocked = h.sender OR blocker = h.sender AND blocked = h.recipient)
ORDER BY s.date DESC
LIMIT @messagesPerPage
OFFSET @offset) m ORDER BY m.date"""

presentMissedContacts ∷ Int → DateTime → Maybe Int → ServerEffect (Array FlatContactHistoryMessage)
presentMissedContacts loggedUserId sinceMessageDate lastSentId = SD.unsafeQuery query
      { loggedUserId
      , sinceMessageDate
      , lastSentId
      , status: Read
      , contacts: Contacts
      }
      where
      query =
            "SELECT" <> presentUserContactFields <> ", " <> presentMessageFields <>
                  """FROM users u
            JOIN karma_leaderboard k ON u.id = k.ranker
            JOIN histories h ON u.id = sender AND recipient = @loggedUserId OR u.id = recipient AND sender = @loggedUserId
            JOIN messages s ON (s.sender = h.sender AND s.recipient = h.recipient OR s.sender = h.recipient AND s.recipient = h.sender)
            LEFT JOIN last_seen ls ON u.id = ls.who
            WHERE visibility <= @contacts
                  AND (last_message_date > @sinceMessageDate AND status < @status OR s.id > @lastSentId AND s.sender = @loggedUserId)
                  AND NOT EXISTS (SELECT 1 FROM blocks WHERE blocker = h.recipient AND blocked = h.sender OR blocker = h.sender AND blocked = h.recipient)
                  AND NOT (h.sender = @loggedUserId AND h.sender_deleted_to IS NOT NULL AND s.id <= h.sender_deleted_to OR
                        h.recipient = @loggedUserId AND h.recipient_deleted_to IS NOT NULL AND s.id <= h.recipient_deleted_to)
            ORDER BY last_message_date DESC, s.date"""

presentUser ∷ Int → ServerEffect (Maybe FlatUser)
presentUser loggedUserId = SD.single $ select userPresentationFields  # from usersSource # wher (u ... _id .=. loggedUserId .&&. _visibility .<>. TemporarilyBanned)