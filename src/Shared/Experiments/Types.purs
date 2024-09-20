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

import Shared.Privilege (Privilege)
import Shared.Unsafe as SU
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
      | RedirectKarma
      | ToggleSection ImpersonationSection
      | UpdatePrivileges { karma ∷ Int, privileges ∷ Array Privilege }

type ChatExperimentModel =
      { experiments ∷ Array ChatExperiment
      , current ∷ Maybe ChatExperiment
      , user ∷ ChatExperimentUser
      --impersonation
      , section ∷ ImpersonationSection
      }

data Experiment = Impersonation | WordChain

data ImpersonationSection
      = HideSections
      | Characters
      | HistoricalFigures
      | Celebrities

derive instance Eq Experiment

derive instance Eq ImpersonationSection

derive instance Ord Experiment

instance Bounded Experiment where
      bottom = Impersonation
      top = WordChain

instance BoundedEnum Experiment where
      cardinality = Cardinality 1
      fromEnum = case _ of
            Impersonation → 0
            WordChain → 10
      toEnum = case _ of
            0 → Just Impersonation
            10 → Just WordChain
            _ → Nothing

instance Enum Experiment where
      succ = case _ of
            Impersonation → Just WordChain
            WordChain → Nothing
      pred = case _ of
            Impersonation → Nothing
            WordChain → Just Impersonation

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
