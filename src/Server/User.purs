module Server.User where

import Prelude
import Server.Types
import Shared.Types

import Data.Date as DD
import Data.Enum as DE
import Effect.Now as EN
import Run as R

toIMUser :: User -> ServerEffect IMUser
toIMUser (User user) = do
        now <- R.liftEffect EN.nowDate
        pure $ IMUser {
                id: user.id,
                name: user.name,
                email: user.email,
                headline: user.headline,
                description: user.description,
                birthday: map ((DE.fromEnum (DD.year now) - _) <<< DE.fromEnum <<< DD.year) user.birthday,
                gender: user.gender,
                recentEmoji: user.recentEmoji,
                country: user.country,
                messageOnEnter: user.messageOnEnter
        }