module Server.IM.Database where

import Prelude
import Server.Types
import Shared.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.String.Common as DS
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (Pool, Query(..), Row1(..))
import Debug.Trace (spy)
import Server.Database as SD
import Shared.Options.Page (contactsPerPage, messagesPerPage, initialMessagesPerPage, suggestionsPerPage)

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

usersTable :: String
usersTable = " users u join karma_leaderboard k on u.id = k.ranker "

messagePresentationFields :: String
messagePresentationFields = " id, sender, recipient, date, content, status "

presentUserQuery :: forall p v. Query p v
presentUserQuery = Query ("select" <> userPresentationFields <> "from" <> usersTable <> "where u.id = $1 and active")

presentUserParameters :: forall t. t -> Row1 t
presentUserParameters = Row1

presentUser :: PrimaryKey -> ServerEffect (Maybe IMUserWrapper)
presentUser loggedUserID = SD.single presentUserQuery $ presentUserParameters loggedUserID

--fit online status here
suggest :: PrimaryKey -> Int -> Maybe ArrayPrimaryKey -> ServerEffect (Array IMUserWrapper)
suggest loggedUserID skip = case _ of
      Just (ArrayPrimaryKey []) ->
            SD.select (Query (select <> rest)) (loggedUserID /\ suggestionsPerPage /\ skip)
      Just (ArrayPrimaryKey keys) ->
            SD.select (Query (select <> "and not (u.id = any($4))" <> rest)) (loggedUserID /\ suggestionsPerPage /\ skip /\ keys)
      _ ->
            SD.select (Query (select <> "and not exists(select 1 from histories where sender in ($1, u.id) and recipient in ($1, u.id))" <> rest)) (loggedUserID /\ suggestionsPerPage /\ skip)
      where select = "select * from (select" <> userPresentationFields <> "from"  <> usersTable <> "join suggestions s on u.id = suggested where u.id <> $1 "
            rest = " and u.active and not exists (select 1 from blocks where blocker in ($1, u.id) and blocked in ($1, u.id)) order by s.id limit $2 offset $3) t order by random()"

presentContacts :: PrimaryKey -> Int -> ServerEffect (Array ContactWrapper)
presentContacts loggedUserID skip = SD.select (Query ("select distinct h.date, sender, date_part('day', age(now() at time zone 'utc', first_message_date)), " <> userPresentationFields <>
                                      "from" <> usersTable <> """join histories h on (u.id = h.sender and h.recipient = $1 or u.id = h.recipient and h.sender = $1)
                                         where not exists (select 1 from blocks where blocker = h.recipient and blocked = h.sender or blocker = h.sender and blocked = h.recipient)
                                          order by date desc limit $2 offset $3""")) (loggedUserID /\ contactsPerPage /\ skip)

--needs to handle impersonations
presentSingleContact :: PrimaryKey -> PrimaryKey -> ServerEffect ContactWrapper
presentSingleContact loggedUserID otherID = SD.single' (Query ("select coalesce(h.date, now() at time zone 'utc'), coalesce(sender, $2), coalesce(first_message_date, now() at time zone 'utc'), " <> userPresentationFields <>
                                      "from" <> usersTable <> "left join histories h on (u.id = h.recipient and h.sender = $1 or u.id = h.sender and h.recipient = $1) where u.id = $2")) (loggedUserID /\ otherID)

presentSelectedContacts :: PrimaryKey -> Array PrimaryKey -> ServerEffect (Array ContactWrapper)
presentSelectedContacts loggedUserID ids
      | DA.null ids = pure []
      | otherwise = SD.select (Query $ "select distinct h.date, sender, first_message_date," <> userPresentationFields <> "from" <> usersTable <> "join histories h on (u.id = h.sender and h.recipient = $1 or u.id = h.recipient and h.sender = $1) where u.id = any($2)") (loggedUserID /\ ids)

--there must be a better way to do this
chatHistoryFor :: PrimaryKey -> Array PrimaryKey -> ServerEffect (Array HistoryMessageWrapper)
chatHistoryFor loggedUserID otherIDs
      | DA.null otherIDs = pure []
      | otherwise = SD.select (Query query) (loggedUserID /\ initialMessagesPerPage /\ Delivered)
      where query = "select * from (" <> DS.joinWith " union all " (select <$> otherIDs) <> ") r order by date, sender, recipient"
            select n =
                  let parameter = show n
                  in "((select" <> messagePresentationFields <> "from messages where sender = $1 and recipient = " <> parameter <> " or sender = " <> parameter <> " and recipient = $1 order by date desc limit $2) union (select" <> messagePresentationFields <> "from messages where recipient = $1 and sender = " <> parameter <> " and status < $3 order by date desc))"

chatHistorySince :: PrimaryKey -> Int -> ServerEffect (Array HistoryMessageWrapper)
chatHistorySince loggedUserID lastID = SD.select (Query $ "select " <> messagePresentationFields <> " from messages m where recipient = $1 and m.id > $2 and status < $3 order by date, sender") (loggedUserID /\ lastID /\ Delivered)

chatHistoryBetween :: PrimaryKey -> PrimaryKey -> Int -> ServerEffect (Array HistoryMessageWrapper)
chatHistoryBetween loggedUserID otherID skip = SD.select (Query ("select * from (select" <> messagePresentationFields <> "from messages where sender = $1 and recipient = $2 or sender = $2 and recipient = $1 order by date desc limit $3 offset $4) s order by date")) (loggedUserID /\ otherID /\ messagesPerPage /\ skip)

messsageIDsFor :: PrimaryKey -> Int -> ServerEffect (Array MessageIDTemporaryWrapper)
messsageIDsFor loggedUserID messageID = SD.select (Query "select id, temporary_id from messages where sender = $1 and id > $2") (loggedUserID /\ messageID)

insertMessage :: forall r. PrimaryKey -> PrimaryKey -> Int -> String -> BaseEffect { pool :: Pool | r } PrimaryKey
insertMessage loggedUserID recipient temporaryID content = SD.withTransaction $ \connection -> do
      SD.executeWith connection (Query """select insert_history($1, $2)""") (loggedUserID /\ recipient)
      SD.insertWith connection (Query """INSERT INTO messages(sender, recipient, temporary_id, content) VALUES ($1, $2, $3, $4)""") (loggedUserID /\ recipient /\ temporaryID /\ content)

insertKarma :: forall r. PrimaryKey -> PrimaryKey -> Tuple Int Int -> BaseEffect { pool :: Pool | r } Unit
insertKarma loggedUserID otherID (Tuple senderKarma recipientKarma) =
      void $ SD.insert (Query "insert into karma_histories(amount, target) values ($1, $2), ($3, $4)") $ ( senderKarma /\ loggedUserID /\ recipientKarma /\ otherID)

--when using an array parameter, any must be used instead of in
changeStatus :: forall r. PrimaryKey -> MessageStatus -> Array PrimaryKey -> BaseEffect { pool :: Pool | r } Unit
changeStatus loggedUserID status ids = SD.execute (Query "update messages set status = $1 where recipient = $2 and id = any($3)") (status /\ loggedUserID /\ ids)

insertBlock :: PrimaryKey -> PrimaryKey -> ServerEffect Unit
insertBlock loggedUserID blocked = void $ SD.insert blockQuery (loggedUserID /\ blocked)

blockQuery = Query "insert into blocks(blocker, blocked) values ($1, $2)"

insertReport :: PrimaryKey -> Report -> ServerEffect Unit
insertReport loggedUserID { userID, comment, reason } = SD.withTransaction $ \connection -> do
      SD.executeWith connection blockQuery (loggedUserID /\ userID)
      SD.executeWith connection (Query """INSERT INTO reports(reporter, reported, reason, comment) VALUES ($1, $2, $3, $4)""") (loggedUserID /\ userID /\ reason /\ comment)
