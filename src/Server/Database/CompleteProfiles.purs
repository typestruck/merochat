module Server.Database.CompleteProfiles where

import Droplet.Language
import Prelude

import Data.Either (Either)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic as DGRS
import Data.Tuple.Nested (type (/\))
import Droplet.Language as DL
import Foreign as F
import Server.Database.Users (UsersTable)
import Shared.Unsafe as SU
import Type.Proxy (Proxy(..))

type CompleteProfiles =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , completer ∷ Column Int (ForeignKey "id" UsersTable)
      , completed ∷ ProfileColumn
      )

type CompleteProfilesTable = Table "complete_profiles" CompleteProfiles

data ProfileColumn
      = Name
      | Headline
      | Description
      | Avatar
      | Birthday
      | Gender
      | Country
      | Languages
      | Tags

derive instance Generic ProfileColumn _

instance Show ProfileColumn where
      show = DGRS.genericShow

derive instance Eq ProfileColumn

derive instance Ord ProfileColumn

instance Bounded ProfileColumn where
      bottom = Name
      top = Birthday

instance BoundedEnum ProfileColumn where
      cardinality = Cardinality 1

      fromEnum = case _ of
            Name → 0
            Headline → 1
            Description → 2
            Avatar → 3
            Birthday → 4
            Gender → 5
            Country → 6
            Languages → 7
            Tags → 8

      toEnum = case _ of
            0 → Just Name
            1 → Just Headline
            2 → Just Description
            3 → Just Avatar
            4 → Just Birthday
            5 → Just Gender
            6 → Just Country
            7 → Just Languages
            8 → Just Tags
            _ → Nothing

instance Enum ProfileColumn where
      succ = case _ of
            Name → Just Headline
            Headline → Just Description
            Description → Just Avatar
            Avatar → Just Birthday
            Birthday → Just Gender
            Gender → Just Country
            Country → Just Languages
            Languages → Just Tags
            Tags → Nothing

      pred = case _ of
            Name → Nothing
            Headline → Just Name
            Description → Just Headline
            Avatar → Just Description
            Birthday → Just Avatar
            Gender → Just Birthday
            Country → Just Gender
            Languages → Just Country
            Tags → Just Languages

instance FromValue ProfileColumn where
      fromValue v = map (SU.fromJust <<< DE.toEnum) (DL.fromValue v ∷ Either String Int)

instance ToValue ProfileColumn where
      toValue = F.unsafeToForeign <<< DE.fromEnum

complete_profiles ∷ CompleteProfilesTable
complete_profiles = Table

_completer ∷ Proxy "completer"
_completer = Proxy

_completed ∷ Proxy "completed"
_completed = Proxy
