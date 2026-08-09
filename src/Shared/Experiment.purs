module Shared.Experiment where

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

data Experiment
      = WordChain
      | Doppelganger
      | PaperPlanes
      | Debate

derive instance Eq Experiment
derive instance Ord Experiment

instance Bounded Experiment where
      bottom = WordChain
      top = Debate

instance BoundedEnum Experiment where
      cardinality = Cardinality 1
      fromEnum = case _ of
            WordChain → 10
            Doppelganger → 20
            PaperPlanes → 30
            Debate → 40
      toEnum = case _ of
            10 → Just WordChain
            20 → Just Doppelganger
            30 → Just PaperPlanes
            40 → Just Debate
            _ → Nothing

instance Enum Experiment where
      succ = case _ of
            WordChain → Just Doppelganger
            Doppelganger → Just PaperPlanes
            PaperPlanes → Just Debate
            Debate → Nothing
      pred = case _ of
            WordChain → Nothing
            Doppelganger → Just WordChain
            PaperPlanes → Just Doppelganger
            Debate → Just PaperPlanes

derive instance Generic Experiment _

instance DecodeJson Experiment where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson Experiment where
      encodeJson = DAEGR.genericEncodeJson

instance ToValue Experiment where
      toValue = F.unsafeToForeign <<< DE.fromEnum

instance FromValue Experiment where
      fromValue v = map (SU.fromJust <<< DE.toEnum) (DL.fromValue v ∷ Either String Int)