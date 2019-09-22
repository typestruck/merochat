module Server.IM.Database where

import Prelude
import Server.Types
import Shared.Types

import Data.Int53 (Int53)
import Database.PostgreSQL (Query(..), Row1(..))
import Server.Database as SD

presentUser :: Int53 -> ServerEffect IMUser
presentUser id =
        SD.single' (Query """select
                u.id,
                gender,
                birthday,
                name,
                headline,
                description,
                (select name from countries where id = country) country,
                (select string_agg(l.id::text, ','  order by name) from languages l join languagesUsers lu on l.id = lu.language and lu.speaker = u.id ) languages,
                (select string_agg(name, '\n' order by name) from tags l join tagsUsers tu on l.id = tu.tag and tu.creator = u.id ) tags
                from users u
                where id = $1""") <<< Row1 $ PrimaryKey id
