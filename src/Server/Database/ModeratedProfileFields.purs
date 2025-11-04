module Server.Database.ModeratedProfileFields where

import Droplet.Language (Column, ForeignKey, Table(..))

import Data.Maybe (Maybe)
import Server.Database.Users (UsersTable)
import Type.Proxy (Proxy(..))

--named like this because droplet does not support joins on same name column fields
type ModeratedProfileFields =
      ( named ∷ String
      , headlined ∷ String
      , descriptioned ∷ String
      , avatared ∷ Maybe String
      , moderated ∷ Column Int (ForeignKey "moderated" UsersTable)
      , chat_backgrounded ∷ Maybe String
      )

type ModeratedProfileFieldsTable = Table "moderated_profile_fields" ModeratedProfileFields

moderated_profile_fields ∷ ModeratedProfileFieldsTable
moderated_profile_fields = Table

_moderated ∷ Proxy "moderated"
_moderated = Proxy

_chatBackgrounded :: Proxy "chat_backgrounded"
_chatBackgrounded = Proxy

_headlined ∷ Proxy "headlined"
_headlined = Proxy

_descriptioned ∷ Proxy "descriptioned"
_descriptioned = Proxy

_avatared ∷ Proxy "avatared"
_avatared = Proxy

_named ∷ Proxy "named"
_named = Proxy


