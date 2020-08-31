module Server.IM.Database where

import Prelude
import Server.Types
import Shared.Types

import Data.Array as DA
import Data.DateTime (DateTime)
import Data.JSDate as DJ
import Data.String.Common as DS
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (Pool, Query(..), Row1(..))
import Server.Database as SD
import Shared.Options.Page (contactsPerPage, messagesPerPage, initialMessagesPerPage)

userPresentationFields :: String
userPresentationFields = """ u.id,
avatar,
gender,
birthday,
name,
headline,
description,
(select name from countries where id = country) country,
(select string_agg(l.name, ','  order by name) from languages l join languagesUsers lu on l.id = lu.language and lu.speaker = u.id ) languages,
(select string_agg(name, '\n' order by name) from tags l join tagsUsers tu on l.id = tu.tag and tu.creator = u.id ) tags,
(select sum(amount) from karmaHistories where target = u.id) karma
 """

messagePresentationFields :: String
messagePresentationFields = " id, sender, recipient, date, content, status "

presentUserQuery :: forall p v. Query p v
presentUserQuery = Query ("select" <> userPresentationFields <> "from users u where id = $1")

presentUserParameters :: forall t. t -> Row1 t
presentUserParameters = Row1

presentUser :: PrimaryKey -> ServerEffect IMUserWrapper
presentUser loggedUserID = SD.single' presentUserQuery $ presentUserParameters loggedUserID

--another thing to think is ordering by online status
suggest :: PrimaryKey -> ServerEffect (Array IMUserWrapper)
suggest loggedUserID =
     SD.select (Query ("select" <> userPresentationFields <> "from users u where id <> $1 and not exists(select 1 from histories where sender in ($1, u.id) and recipient in ($1, u.id)) and not exists (select 1 from blocked where blocker in ($1, u.id) and blocked in ($1, u.id)) order by random() limit 20")) $ Row1 loggedUserID

presentContacts :: PrimaryKey -> Int -> ServerEffect (Array ContactWrapper)
presentContacts loggedUserID skip = SD.select (Query ("select distinct date, sender, firstMessageDate, " <> userPresentationFields <>
                                      """from users u join histories h on (u.id = h.sender and h.recipient = $1 or u.id = h.recipient and h.sender = $1)
                                         where not exists (select 1 from blocked where blocker = h.recipient and blocked = h.sender or blocker = h.sender and blocked = h.recipient)
                                          order by date desc limit $2 offset $3""")) (loggedUserID /\ contactsPerPage /\ skip)

presentSingleContact :: PrimaryKey -> PrimaryKey -> ServerEffect ContactWrapper
presentSingleContact loggedUserID otherID = SD.single' (Query ("select distinct date, sender, firstMessageDate, " <> userPresentationFields <>
                                      """from users u join histories h on (u.id = $1 and h.recipient = $2 or u.id = $2 and h.sender = $1)""")) (loggedUserID /\ otherID)

presentSelectedContacts :: Array PrimaryKey -> ServerEffect (Array ContactWrapper)
presentSelectedContacts ids
      | DA.null ids = pure []
      | otherwise = SD.select (Query $ "select" <> userPresentationFields <> "from users u where id = any($1)") $ Row1 ids

--there must be a better way to do this
chatHistoryFor :: PrimaryKey -> Array PrimaryKey -> ServerEffect (Array HistoryMessageWrapper)
chatHistoryFor loggedUserID otherIDs
      | DA.null otherIDs = pure []
      | otherwise = SD.select (Query query) (loggedUserID /\ initialMessagesPerPage)
      where query = "select * from (" <> DS.joinWith " union all " (select <$> otherIDs) <> ") r order by date, sender, recipient"
            select n =
                  let parameter = show n
                  in "select * from (select" <> messagePresentationFields <> "from messages where sender = $1 and recipient = " <> parameter <> " or sender = " <> parameter <> " and recipient = $1 order by date desc limit $2) a"

chatHistorySince :: PrimaryKey -> DateTime -> ServerEffect (Array HistoryMessageWrapper)
chatHistorySince loggedUserID since = SD.select (Query $ "select " <> messagePresentationFields <> " from messages where recipient = $1 and date >= $2 order by date, sender") (loggedUserID /\ DJ.fromDateTime since)

chatHistoryBetween :: PrimaryKey -> PrimaryKey -> Int -> ServerEffect (Array HistoryMessageWrapper)
chatHistoryBetween loggedUserID otherID skip = SD.select (Query ("select * from (select" <> messagePresentationFields <> "from messages where sender = $1 and recipient = $2 or sender = $2 and recipient = $1 order by date desc limit $3 offset $4) s order by date")) (loggedUserID /\ otherID /\ messagesPerPage /\ skip)

insertMessage :: forall r. PrimaryKey -> PrimaryKey -> String -> BaseEffect { pool :: Pool | r } PrimaryKey
insertMessage loggedUserID recipient content = SD.withTransaction $ \connection -> do
      SD.executeWith connection (Query """select insertHistory($1, $2)""") (loggedUserID /\ recipient)
      SD.insertWith connection (Query """INSERT INTO messages(sender, recipient, content) VALUES ($1, $2, $3)""") (loggedUserID /\ recipient /\ content)

insertKarma :: forall r. PrimaryKey -> PrimaryKey -> Tuple Int Int -> BaseEffect { pool :: Pool | r } Unit
insertKarma loggedUserID otherID (Tuple senderKarma recipientKarma) =
      void $ SD.insert (Query "insert into karmaHistories(amount, target) values ($1, $2), ($3, $4)") $ ( senderKarma /\ loggedUserID /\ recipientKarma /\ otherID)

--when using an array parameter, any must be used instead of in
markRead :: forall r. PrimaryKey -> Array PrimaryKey -> BaseEffect { pool :: Pool | r } Unit
markRead loggedUserID ids = SD.execute (Query "update messages set status = 1 where recipient = $1 and id = any($2)") (loggedUserID /\ ids)

insertBlock :: PrimaryKey -> PrimaryKey -> ServerEffect Unit
insertBlock loggedUserID blocked = void $ SD.insert (Query "insert into blocked(blocker, blocked) values ($1, $2)") (loggedUserID /\ blocked)