module Server.Database.TagsUsers where

import Droplet.Language
import Prim hiding (Constraint)

import Data.Tuple.Nested (type (/\))
import Server.Database.Tags (TagsTable)
import Server.Database.Users (UsersTable)
import Type.Proxy (Proxy(..))

type TagsUsers =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , creator ∷ Column Int (Constraint "tags_user_user" (ForeignKey "id" UsersTable))
      , tag ∷ Column Int (Constraint "tag_user_tag" (ForeignKey "id" TagsTable))
      )

tags_users ∷ Table "tags_users" TagsUsers
tags_users = Table

_creator ∷ Proxy "creator"
_creator = Proxy

_tag ∷ Proxy "tag"
_tag = Proxy
