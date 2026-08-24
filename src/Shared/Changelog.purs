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
import Shared.Experiment (Experiment(..))
import Shared.Profile.Mode (ProfileMode(..))
import Shared.Unsafe as SU

type Changelog =
      { id ∷ Int
      , changed ∷ Maybe Int
      , description ∷ String
      , action ∷ Maybe ChangelogAction
      , value ∷ Maybe Int
      , read ∷ Boolean
      }

data ChangelogAction
      = OpenBackerPage
      | SendDoppelgangerMessage
      | OpenExperimentsPage (Maybe Experiment)
      | OpenProfilePage ProfileMode

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
      top = OpenProfilePage Asked

instance BoundedEnum ChangelogAction where
      cardinality = Cardinality 1

      fromEnum = case _ of
            OpenBackerPage → 0
            SendDoppelgangerMessage → 1
            OpenExperimentsPage (Just PaperPlanes) → 2
            OpenExperimentsPage _ → 4
            OpenProfilePage Edit → 3
            OpenProfilePage OwnPosts → 5
            OpenProfilePage Praise → 6
            OpenProfilePage Asked → 7
            OpenProfilePage Preview → 8

      toEnum = case _ of
            0 → Just OpenBackerPage
            1 → Just SendDoppelgangerMessage
            2 → Just <<< OpenExperimentsPage $ Just PaperPlanes
            3 → Just $ OpenProfilePage Edit
            4 → Just $ OpenExperimentsPage Nothing
            5 → Just $ OpenProfilePage OwnPosts
            6 → Just $ OpenProfilePage Praise
            7 → Just $ OpenProfilePage Asked
            8 → Just $ OpenProfilePage Preview
            _ → Nothing

instance Enum ChangelogAction where
      succ = case _ of
            OpenBackerPage → Just SendDoppelgangerMessage
            SendDoppelgangerMessage → Just $ OpenExperimentsPage Nothing
            OpenExperimentsPage _ → Just $ OpenProfilePage Edit
            OpenProfilePage _ → Nothing

      pred = case _ of
            OpenBackerPage → Nothing
            SendDoppelgangerMessage → Just OpenBackerPage
            OpenExperimentsPage _ → Just SendDoppelgangerMessage
            OpenProfilePage _ → Just $ OpenExperimentsPage Nothing

derive instance Eq ChangelogAction

derive instance Ord ChangelogAction
