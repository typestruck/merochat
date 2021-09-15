module Server.Database.Users where

import Droplet.Language
import Prelude
import Server.Types
import Shared.Types

import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Server.Database as SD
import Server.Database.Fields
import Shared.Account (RegisterLoginUser)
import Shared.User (Gender)
import Type.Proxy (Proxy(..))

type Users =
      ( id ∷ Auto Int
      , password ∷ String
      , name ∷ String
      , headline ∷ String
      , joined ∷ Default DateTime
      , email ∷ String
      , birthday ∷ Maybe Date
      , description ∷ String
      , avatar ∷ Maybe String
      , gender ∷ Maybe Gender
      , country ∷ Maybe Int
      , active ∷ Default Boolean
      )

users ∷ Table "users" Users
users = Table

_password ∷ Proxy "password"
_password = Proxy

_headline ∷ Proxy "headline"
_headline = Proxy

_joined ∷ Proxy "joined"
_joined = Proxy

_email ∷ Proxy "email"
_email = Proxy

_birthday ∷ Proxy "birthday"
_birthday = Proxy

_description ∷ Proxy "description"
_description = Proxy

_avatar ∷ Proxy "avatar"
_avatar = Proxy

_gender ∷ Proxy "gender"
_gender = Proxy

_country ∷ Proxy "country"
_country = Proxy

userBy ∷ By → ServerEffect (Maybe RegisterLoginUser)
userBy = case _ of
      Email value → SD.single $ baseQuery (\ft -> ft .&&. _email .=. value)
      ID value → SD.single $ baseQuery (\ft -> ft .&&. _id .=. value)

baseQuery ft = select (_id /\ _email /\ _password) # from users # wher (ft (_active .=. true))