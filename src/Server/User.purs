module Server.User where

import Prelude
import Server.Types
import Shared.Types

import Data.Date as DD
import Data.Enum as DE
import Effect.Now as EN
import Effect (Effect)

toIMUser :: User -> Effect IMUser
toIMUser (User user) = do
        now <- EN.nowDate
        pure $ IMUser {
                id: PrimaryKey user.id,
                name: user.name,
                email: user.email,
                headline: user.headline,
                description: user.description,
                birthday: map ((DE.fromEnum (DD.year now) - _) <<< DE.fromEnum <<< DD.year) user.birthday,
                gender: user.gender,
                recentEmoji: user.recentEmoji,
                avatar: "/client/media/avatar.png",
                country: map PrimaryKey user.country,
                messageOnEnter: user.messageOnEnter
        }