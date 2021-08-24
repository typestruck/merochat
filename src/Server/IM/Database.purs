module Server.IM.Database where

import Droplet.Language
import Prelude hiding (join)
import Server.Database.Blocks
import Server.Database.Countries
import Server.Database.Fields
import Server.Database.Languages
import Server.Database.LanguagesUsers
import Server.Database.Messages
import Server.Database.Reports
import Server.Database.Tags
import Server.Database.TagsUsers
import Server.Database.Users
import Server.Types
import Shared.IM.Types
import Shared.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.String.Common as DS
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Droplet.Driver (Pool)
import Server.Database as SD
import Server.Database.KarmaLeaderboard
import Shared.Options.Page (contactsPerPage, initialMessagesPerPage, messagesPerPage, suggestionsPerPage)
import Shared.Unsafe as SU
import Shared.User (IU)
import Type.Proxy (Proxy(..))

type FlatContact = BaseContact IU

userPresentationFields :: String
userPresentationFields = """ u.id,
avatar,
gender,
date_part('year', age(now() at time zone 'utc', birthday)) as age,
name,
headline,
description,
(select name from countries where id = country) country,
(select string_agg(l.name, ','  order by name) from languages l join languages_users lu on l.id = lu.language and lu.speaker = u.id ) languages,
(select string_agg(name, '\n' order by l.id) from tags l join tags_users tu on l.id = tu.tag and tu.creator = u.id ) tags,
k.current_karma karma,
k.position """

userPresentationFields2 =
      u ... _id /\
      _avatar /\
      _gender /\
      _birthday /\
      _name /\
      _headline /\
      _description /\
      (select _name # from countries # wher (_id .=. u ... _country) # as _country) /\
      (select (string_agg (l ... _name) (", " # orderBy _name) # as _languages) # from (((languages # as l) `join` (languages_users # as lu)) # on (l ... _id .=. lu ... _language .&&. lu ... _speaker .=. u ... _id))) /\
      (select (string_agg _name ("\n" # orderBy (l ... _id)) # as _tags) # from (((tags # as l) `join` (tags_users # as tu)) # on (l ... _id .=. tu ... _tag .&&. tu ... _creator .=. u ... _id))) /\
      (k ... _current_karma # as _karma) /\
      (k ... _position)

usersTable :: String
usersTable = " users u join karma_leaderboard k on u.id = k.ranker "

messagePresentationFields :: String
messagePresentationFields = " id, sender, recipient, date, content, status "

presentUserQuery :: String
presentUserQuery = "select" <> userPresentationFields <> "from" <> usersTable <> "where u.id = @id and active"

presentUser :: Int -> ServerEffect (Maybe IMUser)
presentUser loggedUserID = SD.unsafeSingle presentUserQuery {id: loggedUserID}

--fit online status here
suggest :: Int -> Int -> Maybe ArrayPrimaryKey -> ServerEffect (Array IMUser)
suggest loggedUserID skip = case _ of
      Just (ArrayPrimaryKey []) ->
            SD.unsafeQuery (select <> rest) {id: loggedUserID, page: suggestionsPerPage, skip}
      Just (ArrayPrimaryKey keys) ->
            SD.unsafeQuery (select <> "and not (u.id = any(@keys))" <> rest) {id: loggedUserID, page: suggestionsPerPage, skip, keys}
      _ ->
            SD.unsafeQuery (select <> "and not exists(select 1 from histories where sender in (@id, u.id) and recipient in (@id, u.id))" <> rest) {id: loggedUserID, page: suggestionsPerPage, skip}
      where select = "select * from (select" <> userPresentationFields <> "from"  <> usersTable <> "join suggestions s on u.id = suggested where u.id <> @id "
            rest = " and u.active and not exists (select 1 from blocks where blocker in (@id, u.id) and blocked in (@id, u.id)) order by s.id limit @page offset @skip) t order by random()"

--should not return contact
presentContacts :: Int -> Int -> ServerEffect (Array FlatContact)
presentContacts loggedUserID skip = SD.unsafeQuery ("select distinct h.date, sender, date_part('day', age(now() at time zone 'utc', first_message_date)), " <> userPresentationFields <>
                                      "from" <> usersTable <> """join histories h on (u.id = h.sender and h.recipient = @sender or u.id = h.recipient and h.sender = @sender)
                                         where not exists (select 1 from blocks where blocker = h.recipient and blocked = h.sender or blocker = h.sender and blocked = h.recipient)
                                          order by date desc limit @page offset @skip""") {sender: loggedUserID, page: contactsPerPage, skip}

--needs to handle impersonations
presentSingleContact :: Int -> Int -> ServerEffect FlatContact
presentSingleContact loggedUserID otherID = SU.fromJust <$> SD.unsafeSingle (("select coalesce(h.date, now() at time zone 'utc'), coalesce(sender, @otherID), coalesce(first_message_date, now() at time zone 'utc'), " <> userPresentationFields <>
                                      "from" <> usersTable <> "left join histories h on (u.id = h.recipient and h.sender = @id or u.id = h.sender and h.recipient = @id) where u.id = @otherID")) {id: loggedUserID, otherID}

presentSelectedContacts :: Int -> Array Int -> ServerEffect (Array FlatContact)
presentSelectedContacts loggedUserID ids
      | DA.null ids = pure []
      | otherwise = SD.unsafeQuery ("select distinct h.date, sender, first_message_date," <> userPresentationFields <> "from" <> usersTable <> "join histories h on (u.id = h.sender and h.recipient = @sender or u.id = h.recipient and h.sender = @sender) where u.id = any(@recipient)") { sender: loggedUserID , recipient: ids }

--there must be a better way to do this
chatHistoryFor :: Int -> Array Int -> ServerEffect (Array HistoryMessage)
chatHistoryFor loggedUserID otherIDs
      | DA.null otherIDs = pure []
      | otherwise = SD.unsafeQuery query {sender: loggedUserID, page: initialMessagesPerPage, status: Delivered}
      where query = "select * from (" <> DS.joinWith " union all " (select <$> otherIDs) <> ") r order by date, sender, recipient"
            select n =
                  let parameter = show n
                  in "((select" <> messagePresentationFields <> "from messages where sender = @sender and recipient = " <> parameter <> " or sender = " <> parameter <> " and recipient = @sender order by date desc limit @page) union (select" <> messagePresentationFields <> "from messages where recipient = @sender and sender = " <> parameter <> " and status < @status order by date desc))"

chatHistorySince :: Int -> Int -> ServerEffect (Array HistoryMessage)
chatHistorySince loggedUserID lastID = SD.query $ select (_id /\ _sender /\ _recipient /\ _date /\ _content /\ _status) # from messages # wher (_recipient .=. loggedUserID .&&. _id .>. lastID .&&. _status .<. Delivered) # orderBy (_date /\ _sender)

chatHistoryBetween :: Int -> Int -> Int -> ServerEffect (Array HistoryMessage)
chatHistoryBetween loggedUserID otherID skip = SD.query $ select star # from (select (_id /\ _sender /\ _recipient /\ _date /\ _content /\ _status) # from messages # wher (_sender .=. loggedUserID .&&. _recipient .=. otherID .||. _sender .=. otherID .&&. _recipient .=. loggedUserID) # orderBy (_date # desc) # limit messagesPerPage # offset skip # as c) # orderBy _date

messsageIDsFor :: Int -> Int -> ServerEffect (Array MessageIDTemporary)
messsageIDsFor loggedUserID messageID = SD.query $ select (_id /\ (_temporary_id # as (Proxy :: Proxy "temporaryID"))) # from messages # wher (_sender .=. loggedUserID .&&. _id .>. messageID)

insertMessage :: forall r. Int -> Int -> Int -> String -> BaseEffect { pool :: Pool | r } Int
insertMessage loggedUserID recipient temporaryID content = SD.withTransaction $ \connection -> do
      SD.unsafeExecuteWith connection "select insert_history(@sender, @recipient)" { sender : loggedUserID, recipient }
      _.id <<< SU.fromJust <$> (SD.singleWith connection $ insert # into messages (_sender /\ _recipient /\ _temporary_id /\ _content) # values (loggedUserID /\ recipient /\ temporaryID /\ content) # returning _id)

insertKarma :: forall r. Int -> Int -> Tuple Int Int -> BaseEffect { pool :: Pool | r } Unit
insertKarma loggedUserID otherID (Tuple senderKarma recipientKarma) =
      void $ SD.unsafeExecute "insert into karma_histories(amount, target) values (@senderKarma, @senderID), (@recipientKarma, @recipientID)" ({senderKarma, senderID: loggedUserID, recipientKarma, recipientID: otherID})

--when using an array parameter, any must be used instead of in
changeStatus :: forall r. Int -> MessageStatus -> Array Int -> BaseEffect { pool :: Pool | r } Unit
changeStatus loggedUserID status ids = SD.execute $ update messages # set (_status /\ status) # wher (_recipient .=. loggedUserID .&&. (_id `in_` ids))

insertBlock :: Int -> Int -> ServerEffect Unit
insertBlock loggedUserID blocked = SD.execute $ blockQuery loggedUserID blocked

blockQuery :: Int -> Int -> _
blockQuery blocker blocked = insert # into blocks (_blocker /\ _blocked) # values (blocker /\ blocked)

insertReport :: Int -> Report -> ServerEffect Unit
insertReport loggedUserID { userID, comment, reason } = SD.withTransaction $ \connection -> do
      SD.executeWith connection $ blockQuery loggedUserID userID
      SD.executeWith connection $ insert # into reports (_reporter /\ _reported /\ _reason /\ _comment) # values (loggedUserID /\ userID /\ reason /\ comment)
