module Shared.Experiments.Types where

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
import Shared.Modal.Types (ScreenModal)
import Shared.Privilege (Privilege)
import Shared.Unsafe as SU
import Shared.User (IU)
import Simple.JSON (class ReadForeign, class WriteForeign)

type ChatExperiment =
      { id ∷ Int
      , name ∷ String
      , description ∷ String
      , code ∷ Experiment
      }

type ChatExperimentUser = { privileges ∷ Array Privilege }

data ChatExperimentMessage
      = QuitExperiment
      | JoinExperiment ChatExperiment
      | ConfirmExperiment (Maybe Experiment)
      | ToggleVisibility ScreenModal
      | RedirectKarma
      | ToggleSection ImpersonationSection
      | UpdatePrivileges { karma ∷ Int, privileges ∷ Array Privilege }

type ChatExperimentModel =
      { experiments ∷ Array ChatExperiment
      , confirming ∷ Maybe Experiment
      , current ∷ Maybe Experiment
      , user ∷ ChatExperimentUser
      , visible :: Boolean
      , section ∷ ImpersonationSection
      }

data Experiment = Impersonation (Maybe ImpersonationProfile) | WordChain | Doppelganger

type ImpersonationProfile = Record IU

data ImpersonationSection
      = HideSections
      | Characters
      | HistoricalFigures
      | Celebrities

derive instance Eq Experiment
derive instance Eq ImpersonationSection

derive instance Ord Experiment

instance Bounded Experiment where
      bottom = Impersonation Nothing
      top = Doppelganger

instance BoundedEnum Experiment where
      cardinality = Cardinality 1
      fromEnum = case _ of
            Impersonation _ → 0
            WordChain → 10
            Doppelganger → 20
      toEnum = case _ of
            0 → Just (Impersonation Nothing)
            10 → Just WordChain
            20 → Just Doppelganger
            _ → Nothing

instance Enum Experiment where
      succ = case _ of
            Impersonation _ → Just WordChain
            WordChain → Just Doppelganger
            Doppelganger → Nothing
      pred = case _ of
            Impersonation _ → Nothing
            WordChain → Just (Impersonation Nothing)
            Doppelganger → Just WordChain

derive instance Generic Experiment _

instance DecodeJson Experiment where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson Experiment where
      encodeJson = DAEGR.genericEncodeJson

instance ReadForeign Experiment where
      readImpl f = SU.fromJust <<< DE.toEnum <$> F.readInt f

instance WriteForeign Experiment where
      writeImpl = F.unsafeToForeign <<< DE.fromEnum

instance ToValue Experiment where
      toValue = F.unsafeToForeign <<< DE.fromEnum

instance FromValue Experiment where
      fromValue v = map (SU.fromJust <<< DE.toEnum) (DL.fromValue v ∷ Either String Int)

derive instance Generic ImpersonationSection _

instance EncodeJson ImpersonationSection where
      encodeJson = DAEGR.genericEncodeJson

instance DecodeJson ImpersonationSection where
      decodeJson = DADGR.genericDecodeJson
