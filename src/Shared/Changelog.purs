module Shared.Changelog where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Either (Either)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Droplet.Language (class FromValue, class ToValue)
import Droplet.Language as DL
import Foreign as F
import Shared.Unsafe as SU

type Changelog =
      { id ∷ Int
      , changed ∷ Maybe Int
      , description ∷ String
      , action ∷ Maybe ChangelogAction
      , value ∷ Maybe Int
      , read ∷ Boolean
      }

data ChangelogAction = OpenBackerPage | SendDoppelgangerMessage | OpenExperimentsPage

derive instance Generic ChangelogAction _

instance EncodeJson ChangelogAction where
      encodeJson = DAEGR.genericEncodeJson

instance DecodeJson ChangelogAction where
      decodeJson = DADGR.genericDecodeJson

instance FromValue ChangelogAction where
      fromValue v = map (SU.fromJust <<< DE.toEnum) (DL.fromValue v ∷ Either String Int)

instance ToValue ChangelogAction where
      toValue = F.unsafeToForeign <<< DE.fromEnum

instance Bounded ChangelogAction where
      bottom = OpenBackerPage
      top = OpenExperimentsPage

instance BoundedEnum ChangelogAction where
      cardinality = Cardinality 1

      fromEnum = case _ of
            OpenBackerPage → 0
            SendDoppelgangerMessage → 1
            OpenExperimentsPage → 2

      toEnum = case _ of
            0 → Just OpenBackerPage
            1 → Just SendDoppelgangerMessage
            2 → Just OpenExperimentsPage
            _ → Nothing

instance Enum ChangelogAction where
      succ = case _ of
            OpenBackerPage → Just SendDoppelgangerMessage
            SendDoppelgangerMessage → Just OpenExperimentsPage
            OpenExperimentsPage → Nothing

      pred = case _ of
            OpenBackerPage → Nothing
            SendDoppelgangerMessage → Just OpenBackerPage
            OpenExperimentsPage → Just SendDoppelgangerMessage

derive instance Eq ChangelogAction

derive instance Ord ChangelogAction
