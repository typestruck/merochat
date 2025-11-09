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
import Data.Tuple.Nested (type (/\), (/\))
import Droplet.Language (class FromValue, class ToValue)
import Droplet.Language as DL
import Foreign as F
import Shared.Modal (ScreenModal)
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

data ExperimentsMessage
      = ToggleVisibility ScreenModal
      | RedirectKarma
      | ToggleSection ShowingExperiment
      | UpdatePrivileges { karma ∷ Int, privileges ∷ Array Privilege }
      | ResumeQuestions
      | SelectChoice Int Int
      | AnswerQuestion
      | AfterAnswerQuestion
      | DisplayQuestions (Array Question)
      | FetchMatches
      | DisplayMatches (Array Match)
      | MessageDoppelganger Int

type Match = { name ∷ String, id ∷ Int }

type ExperimentsModel =
      { experiments ∷ Array ChatExperiment
      , user ∷ ChatExperimentUser
      , visible ∷ Boolean
      , section ∷ ShowingExperiment
      , doppelganger ∷
              { questions ∷ Array Question
              , loading ∷ Boolean
              , selectedChoice ∷ Maybe { question ∷ Int, choice ∷ Int }
              , matches ∷ Array Match
              , completed ∷ Boolean
              }
      }

type Choice =
      { id ∷ Int
      , description ∷ String
      , chosen ∷ Boolean
      }

type Question =
      { id ∷ Int
      , description ∷ String
      , choices ∷ Array Choice
      }

data Experiment = WordChain | Doppelganger

data ShowingExperiment = HideExperiments | ShowingDoppelganger DoppelgangerSection

data DoppelgangerSection
      = ShowDoppelganger
      | ShowNextQuestion
      | ShowMatches

derive instance Eq Experiment

derive instance Ord Experiment

instance Bounded Experiment where
      bottom = WordChain
      top = Doppelganger

instance BoundedEnum Experiment where
      cardinality = Cardinality 1
      fromEnum = case _ of
            WordChain → 10
            Doppelganger → 20
      toEnum = case _ of
            10 → Just WordChain
            20 → Just Doppelganger
            _ → Nothing

instance Enum Experiment where
      succ = case _ of
            WordChain → Just Doppelganger
            Doppelganger → Nothing
      pred = case _ of
            WordChain → Nothing
            Doppelganger → Just WordChain

derive instance Generic Experiment _
derive instance Generic ShowingExperiment _
derive instance Generic DoppelgangerSection _

instance DecodeJson ShowingExperiment where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson DoppelgangerSection where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson Experiment where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson Experiment where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson ShowingExperiment where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson DoppelgangerSection where
      encodeJson = DAEGR.genericEncodeJson

instance ReadForeign Experiment where
      readImpl f = SU.fromJust <<< DE.toEnum <$> F.readInt f

instance WriteForeign Experiment where
      writeImpl = F.unsafeToForeign <<< DE.fromEnum

instance ToValue Experiment where
      toValue = F.unsafeToForeign <<< DE.fromEnum

instance FromValue Experiment where
      fromValue v = map (SU.fromJust <<< DE.toEnum) (DL.fromValue v ∷ Either String Int)