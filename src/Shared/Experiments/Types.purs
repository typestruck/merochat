module Shared.Experiments.Types where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic as DGRS
import Droplet.Language (class FromValue)
import Shared.Privilege (Privilege)
import Shared.User (IU)

type ImpersonationProfile = Record IU

type BaseChatExperiment fields =
      { id ∷ Int
      , name ∷ String
      , description ∷ String
      | fields
      }

type ChatExperiment = BaseChatExperiment (code ∷ ExperimentData)

type ChatExperimentUser = { privileges ∷ Array Privilege }

data ChatExperimentMessage
      = QuitExperiment
      | JoinExperiment ExperimentData
      | ToggleSection ChatExperimentSection
      | ConfirmImpersonation (Maybe ImpersonationProfile)
      | RedirectKarma

data ChatExperimentSection
      = HideSections
      | Characters
      | HistoricalFigures
      | Celebrities

type ChatExperimentModel =
      { experiments ∷ Array ChatExperiment
      , section ∷ ChatExperimentSection
      , current ∷ Maybe ExperimentData
      , impersonation ∷ Maybe ImpersonationProfile
      , user ∷ ChatExperimentUser
      }

--refactor: this type is being used in a very bonkers way, pls fix his shit
data ExperimentData = Impersonation (Maybe ImpersonationProfile)

data ExperimentPayload = ImpersonationPayload
      { id ∷ Int
      , sender ∷ Boolean
      }

derive instance Generic ChatExperimentSection _
derive instance Generic ExperimentData _
derive instance Generic ExperimentPayload _

derive instance Eq ChatExperimentSection
derive instance Eq ExperimentData

instance EncodeJson ExperimentPayload where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson ExperimentData where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson ChatExperimentSection where
      encodeJson = DAEGR.genericEncodeJson

instance DecodeJson ExperimentData where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson ExperimentPayload where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson ChatExperimentSection where
      decodeJson = DADGR.genericDecodeJson

derive instance Ord ExperimentData

instance Bounded ExperimentData where
      bottom = Impersonation Nothing
      top = Impersonation Nothing

instance BoundedEnum ExperimentData where
      cardinality = Cardinality 1
      fromEnum = case _ of
            Impersonation _ → 0
      toEnum = case _ of
            0 → Just $ Impersonation Nothing
            _ → Nothing

instance Enum ExperimentData where
      succ = case _ of
            Impersonation _ → Nothing
      pred = case _ of
            Impersonation _JsonBoolean → Nothing

instance Show ExperimentPayload where
      show = DGRS.genericShow

instance FromValue ExperimentData where
      fromValue = Right <<< const (Impersonation Nothing)