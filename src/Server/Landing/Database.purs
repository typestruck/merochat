module Server.Landing.Database where

import Server.Types
import Shared.Types
import Prelude
import Server.Database as SD
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (Query(..), Row1(..))

createUser :: { email :: String, name :: String, password :: String, headline :: String, description :: String } -> ServerEffect PrimaryKey
createUser user = SD.withTransaction $ \connection -> do
        userID <- SD.insertWith connection (Query """INSERT INTO users(name, password, email, headline, description) VALUES ($1, $2, $3, $4, $5)""") (user.name /\ user.password /\ user.email /\ user.headline /\ user.description)
        _ :: PrimaryKey <- SD.insertWith connection (Query "insert into karmaHistories(amount, target) values (5, $1)") $ Row1 userID
        --use the median score as new user suggestion score so they are not thrown to the bottom of the pile
        _ :: PrimaryKey <- SD.insertWith connection (Query "insert into suggestions(suggested, score) values ($1, coalesce((select score from suggestions order by id limit 1 offset ((select count(*) from suggestions) / 2)), 0))") $ Row1 userID
        pure userID