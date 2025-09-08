module Server.Database.CompleteProfiles where

import Droplet.Language (Column, ForeignKey, Identity, PrimaryKey, Table(..))

import Data.Tuple.Nested (type (/\))
import Shared.ProfileColumn (ProfileColumn)
import Server.Database.Users (UsersTable)
import Type.Proxy (Proxy(..))

type CompleteProfiles =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , completer ∷ Column Int (ForeignKey "id" UsersTable)
      , completed ∷ ProfileColumn
      )

type CompleteProfilesTable = Table "complete_profiles" CompleteProfiles

complete_profiles ∷ CompleteProfilesTable
complete_profiles = Table

_completer ∷ Proxy "completer"
_completer = Proxy

_completed ∷ Proxy "completed"
_completed = Proxy
