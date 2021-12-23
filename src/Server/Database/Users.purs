module Server.Database.Users where

import Droplet.Language
import Prelude
import Server.Database.Fields
import Server.Types
import Shared.ContentType

import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Server.Database as SD
import Server.Database.Countries (CountriesTable)
import Server.Database.Types (Checked)
import Shared.Account (RegisterLoginUser)
import Shared.User (Gender, ProfileVisibility(..))
import Type.Proxy (Proxy(..))

type Users =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , password ∷ String
      , name ∷ String
      , headline ∷ String
      , joined ∷ Column DateTime Default
      , email ∷ String
      , birthday ∷ Maybe Date
      , description ∷ String
      , avatar ∷ Maybe String
      , gender ∷ Maybe Gender
      , country ∷ Column (Maybe Int) (ForeignKey "id" CountriesTable)
      , read_receipts ∷ Column Checked Default
      , typing_status ∷ Column Checked Default
      , online_status ∷ Column Checked Default
      , message_timestamps ∷ Column Checked Default
      , visibility ∷ Column ProfileVisibility Default
      )

type UsersTable = Table "users" Users

users ∷ UsersTable
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

_visibility ∷ Proxy "visibility"
_visibility = Proxy

_readReceipts ∷ Proxy "read_receipts"
_readReceipts = Proxy

_typingStatus ∷ Proxy "typing_status"
_typingStatus = Proxy

_onlineStatus ∷ Proxy "online_status"
_onlineStatus = Proxy

_messageTimestamps ∷ Proxy "message_timestamps"
_messageTimestamps = Proxy

userBy ∷ By → ServerEffect (Maybe RegisterLoginUser)
userBy = case _ of
      Email value → SD.single $ baseQuery (\ft → ft .&&. _email .=. value)
      ID value → SD.single $ baseQuery (\ft → ft .&&. _id .=. value)

baseQuery ft = select (_id /\ _email /\ _password) # from users # wher (ft (_visibility .<>. TemporarilyBanned))