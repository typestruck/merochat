module Shared.Profile.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Show.Generic as DGRS
import Data.String as DS
import Data.String.Read (class Read)
import Data.String.Read as DSR
import Foreign as F
import Foreign.Object (Object)
import Foreign.Object as FO
import Payload.Client.QueryParams (class EncodeQueryParam)
import Payload.Server.QueryParams (class DecodeQueryParam, DecodeError(..))
import Shared.Experiments.Types (ExperimentData)
import Shared.IM.Types (DateWrapper)
import Shared.Network (RequestStatus)
import Shared.Unsafe as SU
import Shared.User (BasicUser, Gender)
import Simple.JSON (class ReadForeign, class WriteForeign)

--used to generically set records
type ProfileModel = Record PM

data ProfileMessage
      = SetPField (ProfileModel → ProfileModel)
      | SelectAvatar
      | SetAvatar String
      | Save Field
      | SetProfileChatExperiment (Maybe ExperimentData)

--this sucks
data Field = Generated What

data What
      = Name
      | Headline
      | Description

type PU =
      ( BasicUser
              ( gender ∷ Maybe Gender
              , country ∷ Maybe Int
              , languages ∷ Array Int
              , age ∷ Maybe DateWrapper
              )
      )

type ProfileUser = Record PU

type GeneratedInput = { field ∷ What, value ∷ Maybe String }

type Choice = Maybe

type PM =
      ( user ∷ ProfileUser
      , nameInputed ∷ Maybe String
      , headlineInputed ∷ Maybe String
      , ageInputed ∷ Choice (Maybe DateWrapper)
      , genderInputed ∷ Choice (Maybe Gender)
      , countryInputed ∷ Choice (Maybe Int)
      , languagesInputed ∷ Maybe Int
      , languagesInputedList ∷ Maybe (Array Int)
      , tagsInputed ∷ Maybe String
      , tagsInputedList ∷ Maybe (Array String)
      , descriptionInputed ∷ Maybe String
      , loading ∷ Boolean
      , countries ∷ Array { id ∷ Int, name ∷ String }
      , languages ∷ Array { id ∷ Int, name ∷ String }
      , updateRequestStatus ∷ Maybe RequestStatus
      , experimenting ∷ Maybe ExperimentData
      )

derive instance Generic Field _
derive instance Generic What _

derive instance Eq Field
derive instance Eq What

derive instance Ord What

instance Show Field where
      show = DGRS.genericShow

instance Show What where
      show = DGRS.genericShow

instance EncodeJson Field where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson What where
      encodeJson = DAEGR.genericEncodeJson

instance DecodeJson Field where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson What where
      decodeJson = DADGR.genericDecodeJson

instance ReadForeign What where
      readImpl value = SU.fromJust <<< DE.toEnum <$> F.readInt value

instance WriteForeign What where
      writeImpl = F.unsafeToForeign <<< DE.fromEnum

instance Bounded What where
      bottom = Name
      top = Description

instance BoundedEnum What where
      cardinality = Cardinality 1
      fromEnum = case _ of
            Name → 0
            Headline → 1
            Description → 2
      toEnum = case _ of
            0 → Just Name
            1 → Just Headline
            2 → Just Description
            _ → Nothing

instance Enum What where
      succ = case _ of
            Name → Just Headline
            Headline → Just Description
            Description  → Nothing
      pred = case _ of
            Name → Nothing
            Headline → Just Name
            Description → Just Headline
