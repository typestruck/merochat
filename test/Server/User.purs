module Test.Server.User where

import Prelude
import Server.Database as SD
import Data.Maybe(Maybe(..))
import Server.Types

userCount :: ServerEffect Int
userCount = do
    count <- SD.unsafeSingle ("select count(1) as count from users") {} :: BaseEffect _ (Maybe { count :: Int })
    pure $ case count of
        Just { count } -> count
        Nothing -> 0

email :: String
email = "e@a.com"

password :: String
password = "hunter12"