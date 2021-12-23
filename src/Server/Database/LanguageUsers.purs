module Server.Database.LanguagesUsers where

import Droplet.Language

import Data.Tuple.Nested (type (/\))
import Server.Database.Users (UsersTable)
import Type.Proxy (Proxy(..))
import Server.Database.Languages (LanguagesTable)

type LanguagesUsers =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , speaker ∷ Column Int (ForeignKey "id" UsersTable)
      , language ∷ Column Int (ForeignKey "id" LanguagesTable)
      )

languages_users ∷ Table "languages_users" LanguagesUsers
languages_users = Table

_speaker ∷ Proxy "speaker"
_speaker = Proxy

_language ∷ Proxy "language"
_language = Proxy