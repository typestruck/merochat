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
import Shared.Modal (ScreenModal)
import Shared.Privilege (Privilege)
import Shared.Unsafe as SU
import Web.Event.Internal.Types (Event)

type ChatExperiment =
      { id ∷ Int
      , name ∷ String
      , description ∷ String
      , code ∷ Experiment
      }

type ChatExperimentUser = { id :: Int, privileges ∷ Array Privilege }

data ExperimentsMessage
      = ToggleVisibility ScreenModal
      | RedirectKarma
      | UpdatePrivileges { karma ∷ Int, privileges ∷ Array Privilege }

      | ResumeQuestions
      | SelectChoice Int Int
      | AnswerQuestion
      | AfterAnswerQuestion
      | DisplayQuestions (Array Question)
      | FetchMatches
      | ToggleDoppelgangerSection DoppelgangerSection
      | DisplayMatches (Array Match)
      | MessageDoppelganger Int

      | SetPlaneMessage String
      | ThrowPlane
      | AfterThrowPlane Int
      | ResizeMessageInput Event
      | TogglePaperPlaneSection PaperPlaneSection
      | DisplayFlyingPaperPlanes (Array PaperPlane)
      | CatchPaperPlane Int
      | AfterCatchPlane Int
      | PassPaperPlane Int
      | AfterPassPlane Int
      | ReportPlane Int Int

type Match =
      { name ∷ String
      , id ∷ Int
      }

type PaperPlane =
      { id ∷ Int
      , message ∷ String
      , thrower :: Int
      , status ∷ PaperPlaneStatus
      }

type ExperimentsModel =
      { experiments ∷ Array ChatExperiment
      , user ∷ ChatExperimentUser
      , visible ∷ Boolean
      , doppelganger ∷
              { questions ∷ Array Question
              , loading ∷ Boolean
              , selectedChoice ∷ Maybe { question ∷ Int, choice ∷ Int }
              , matches ∷ Array Match
              , section ∷ DoppelgangerSection
              , completed ∷ Boolean
              }
      , paperPlane ∷
              { loading ∷ Boolean
              , message ∷ Maybe String
              , section ∷ PaperPlaneSection
              , thrown ∷ Array PaperPlane
              , flyingBy ∷ Array PaperPlane
              , caught ∷ Array PaperPlane
              }
      }

data PaperPlaneStatus
      = Flying
      | Caught
      | Crashed

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

data Experiment = WordChain | Doppelganger | PaperPlanes

data PaperPlaneSection
      = ShowNew
      | ShowFlyingBy
      | ShowCaught

data DoppelgangerSection
      = ShowDoppelganger
      | ShowNextQuestion
      | ShowMatches

derive instance Eq Experiment
derive instance Eq PaperPlaneStatus
derive instance Eq PaperPlaneSection

derive instance Ord Experiment
derive instance Ord PaperPlaneStatus

instance Bounded PaperPlaneStatus where
      bottom = Flying
      top = Crashed

instance Bounded Experiment where
      bottom = WordChain
      top = PaperPlanes

instance BoundedEnum PaperPlaneStatus where
      cardinality = Cardinality 1
      fromEnum = case _ of
            Flying → 1
            Caught → 2
            Crashed → 3
      toEnum = case _ of
            1 → Just Flying
            2 → Just Caught
            3 → Just Crashed
            _ → Nothing

instance BoundedEnum Experiment where
      cardinality = Cardinality 1
      fromEnum = case _ of
            WordChain → 10
            Doppelganger → 20
            PaperPlanes → 30
      toEnum = case _ of
            10 → Just WordChain
            20 → Just Doppelganger
            30 → Just PaperPlanes
            _ → Nothing

instance Enum PaperPlaneStatus where
      succ = case _ of
            Flying → Just Caught
            Caught → Just Crashed
            Crashed → Nothing
      pred = case _ of
            Flying → Nothing
            Caught → Just Flying
            Crashed → Just Caught

instance Enum Experiment where
      succ = case _ of
            WordChain → Just Doppelganger
            Doppelganger → Just PaperPlanes
            PaperPlanes → Nothing
      pred = case _ of
            WordChain → Nothing
            Doppelganger → Just WordChain
            PaperPlanes → Just Doppelganger

derive instance Generic Experiment _
derive instance Generic DoppelgangerSection _
derive instance Generic PaperPlaneStatus _
derive instance Generic PaperPlaneSection _

instance DecodeJson PaperPlaneSection where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson DoppelgangerSection where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson PaperPlaneStatus where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson Experiment where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson Experiment where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson PaperPlaneStatus where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson PaperPlaneSection where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson DoppelgangerSection where
      encodeJson = DAEGR.genericEncodeJson

instance ToValue Experiment where
      toValue = F.unsafeToForeign <<< DE.fromEnum

instance ToValue PaperPlaneStatus where
      toValue = F.unsafeToForeign <<< DE.fromEnum

instance FromValue PaperPlaneStatus where
      fromValue v = map (SU.fromJust <<< DE.toEnum) (DL.fromValue v ∷ Either String Int)

instance FromValue Experiment where
      fromValue v = map (SU.fromJust <<< DE.toEnum) (DL.fromValue v ∷ Either String Int)

instance Show PaperPlaneStatus where
      show = case _ of
            Flying → "Flying"
            Caught → "Caught"
            Crashed → "Crashed"