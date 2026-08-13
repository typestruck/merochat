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
import Shared.Experiment
import Shared.Unsafe as SU
import Web.Event.Internal.Types (Event)

type ChatExperiment =
      { id ∷ Int
      , name ∷ String
      , description ∷ String
      , code ∷ Experiment
      }

type ChatExperimentUser = { id ∷ Int, privileges ∷ Array Privilege }

data ExperimentsMessage
      = ToggleVisibility ScreenModal
      | RedirectKarma
      | UpdatePrivileges { karma ∷ Int, privileges ∷ Array Privilege }
      | SetCurrentExperiment Experiment

      | ResumeQuestions
      | SelectChoice Int Int
      | AnswerQuestion
      | AfterAnswerQuestion
      | DisplayQuestions (Array Question)
      | FetchMatches
      | ToggleDoppelgangerSection DoppelgangerSection
      | DisplayMatches (Array Match)
      | MessageFromExperiment Int

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
      | MessagePaperPlane Int String

      | ToggleDebateSection DebateSection
      | SetDebateTopic String
      | SetDebateStatement String
      | SetInFavor String
      | StartDebate
      | AfterStartDebate Int
      | ToggleFormat

type Match =
      { name ∷ String
      , id ∷ Int
      }

type PaperPlane =
      { id ∷ Int
      , message ∷ String
      , thrower ∷ Int
      , name ∷ String
      , status ∷ PaperPlaneStatus
      }

type Debate =
      { id ∷ Int
      , topic ∷ String
      , pro ∷ Maybe Int
      , con ∷ Maybe Int
      }

type ExperimentsModel =
      { experiments ∷ Array ChatExperiment
      , user ∷ ChatExperimentUser
      , visible ∷ Boolean
      , current ∷ Maybe Experiment
      , doppelganger ∷
              { questions ∷ Array Question
              , loading ∷ Boolean
              , selectedChoice ∷ Maybe { question ∷ Int, choice ∷ Int }
              , matches ∷ Array Match
              , section ∷ DoppelgangerSection
              , completed ∷ Boolean
              }
      , debate ∷
              { section ∷ DebateSection
              , loading ∷ Boolean
              , topic ∷ Maybe String
              , showFormat :: Boolean
              , inFavor ∷ Maybe Boolean
              , statement ∷ Maybe String
              , mine ∷ Array Debate

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

data PaperPlaneSection
      = ShowNew
      | ShowFlyingBy
      | ShowCaught

data DebateSection
      = ShowNewDebate
      | ShowJoin
      | ShowPublic
      | ShowMine

data DoppelgangerSection
      = ShowDoppelganger
      | ShowNextQuestion
      | ShowMatches

derive instance Eq PaperPlaneStatus
derive instance Eq DebateSection
derive instance Eq PaperPlaneSection

derive instance Ord PaperPlaneStatus

instance Bounded PaperPlaneStatus where
      bottom = Flying
      top = Crashed

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

instance Enum PaperPlaneStatus where
      succ = case _ of
            Flying → Just Caught
            Caught → Just Crashed
            Crashed → Nothing
      pred = case _ of
            Flying → Nothing
            Caught → Just Flying
            Crashed → Just Caught

derive instance Generic DebateSection _
derive instance Generic DoppelgangerSection _
derive instance Generic PaperPlaneStatus _
derive instance Generic PaperPlaneSection _

instance DecodeJson PaperPlaneSection where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson DebateSection where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson DoppelgangerSection where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson PaperPlaneStatus where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson DebateSection where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson PaperPlaneStatus where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson PaperPlaneSection where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson DoppelgangerSection where
      encodeJson = DAEGR.genericEncodeJson

instance ToValue PaperPlaneStatus where
      toValue = F.unsafeToForeign <<< DE.fromEnum

instance FromValue PaperPlaneStatus where
      fromValue v = map (SU.fromJust <<< DE.toEnum) (DL.fromValue v ∷ Either String Int)

instance Show PaperPlaneStatus where
      show = case _ of
            Flying → "Flying"
            Caught → "Caught"
            Crashed → "Crashed"