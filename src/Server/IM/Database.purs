module Server.IM.Database where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (class FromSQLRow, Pool, Query(..), Row1(..), Row2(..), Row3(..))
import Server.Database as SD
import Server.Types (ServerEffect, BaseEffect)
import Shared.IM.Types (Contact(..), HistoryMessage, IMUser)
import Shared.Types (PrimaryKey)

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

presentUser :: PrimaryKey -> ServerEffect IMUser
presentUser id = SD.single' presentUserQuery $ presentUserParameters id

--another thing to think is ordering by online status
suggest :: PrimaryKey -> ServerEffect (Array IMUser)
suggest id =
      SD.select (Query ("select" <> userPresentationFields <> "from users u where id not in (1, $1) and not exists(select 1 from histories where sender in ($1, u.id) and recipient in ($1, u.id)) order by random() limit 20")) $ Row1 id

presentContacts :: PrimaryKey -> Int -> ServerEffect (Array Contact)
presentContacts id page = SD.select (Query ("select distinct date, sender, firstMessageDate, " <> userPresentationFields <>
                                      """from users u join histories h on (u.id = h.sender and h.recipient = $1 or u.id = h.recipient and h.sender = $1)
                                          order by date desc limit 10 offset $2""")) (id /\ page * 10)

chatHistory :: PrimaryKey -> Array PrimaryKey -> ServerEffect (Array HistoryMessage)
chatHistory id otherIDs = SD.select (Query ("select" <> messagePresentationFields <> "from messages where (sender = $1 or recipient = $1) and (sender = any($2) or recipient = any($2)) order by date, sender, recipient")) (id /\ otherIDs)

insertMessage :: forall r. PrimaryKey -> PrimaryKey -> String -> BaseEffect { pool :: Pool | r } (Tuple PrimaryKey (Either IMUser PrimaryKey))
insertMessage sender recipient content = SD.withTransaction $ \connection -> do
      priorExistingHistory <- SD.scalarWith connection (Query """select insertHistory($1, $2)""") $ Row2 sender recipient
      messageID <- SD.insertWith connection (Query """INSERT INTO messages(sender, recipient, content) VALUES ($1, $2, $3)""") $ Row3 sender recipient content
      if priorExistingHistory then
              pure <<< Tuple messageID $ Right sender
       else do
              senderUser <- SD.singleWith connection presentUserQuery $ presentUserParameters sender
              pure <<< Tuple messageID $ Left senderUser

--when using an array parameter, any must be used instead of in
markRead :: forall r. PrimaryKey -> Array PrimaryKey -> BaseEffect { pool :: Pool | r } Unit
markRead recipient ids = SD.execute (Query "update messages set status = 1 where recipient = $1 and id = any($2)") $ Row2 recipient ids