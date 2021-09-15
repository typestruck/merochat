module Shared.Profile.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Show.Generic as DGRS
import Data.String as DS
import Data.String.Read (class Read)
import Data.String.Read as DSR
import Foreign.Object (Object)
import Foreign.Object as FO
import Payload.Client.QueryParams (class EncodeQueryParam)
import Payload.Server.QueryParams (class DecodeQueryParam, DecodeError(..))
import Shared.Experiments.Types (ExperimentData)
import Shared.IM.Types (DateWrapper)
import Shared.User (BasicUser, Gender)

--used to generically set records
type ProfileModel = Record PM

data ProfileMessage
      = SetPField (ProfileModel → ProfileModel)
      | SelectAvatar
      | SetAvatar String
      | SetGenerate Generate
      | SetProfileChatExperiment (Maybe ExperimentData)
      | SaveProfile

data Generate
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
      , generating ∷ Maybe Generate
      , countries ∷ Array { id ∷ Int, name ∷ String }
      , languages ∷ Array { id ∷ Int, name ∷ String }
      , hideSuccessMessage ∷ Boolean
      , experimenting ∷ Maybe ExperimentData
      )

derive instance genericGenerate ∷ Generic Generate _

derive instance eqGenerate ∷ Eq Generate

instance showGenerate ∷ Show Generate where
      show = DGRS.genericShow

instance EncodeQueryParam Generate where
      encodeQueryParam = Just <<< show

instance decodeQueryGenerate ∷ DecodeQueryParam Generate where
      decodeQueryParam query key =
            case FO.lookup key query of
                  Nothing → Left $ QueryParamNotFound { key, queryObj: query }
                  Just [ value ] → DM.maybe (errorDecoding query key) Right $ DSR.read value
                  _ → errorDecoding query key

instance encodeJsonGenerate ∷ EncodeJson Generate where
      encodeJson = DAEGR.genericEncodeJson

instance decodeJsonGenerate ∷ DecodeJson Generate where
      decodeJson = DADGR.genericDecodeJson

--refactor: get rid of read instances
instance readGenerate ∷ Read Generate where
      read input =
            case DS.toLower $ DS.trim input of
                  "name" → Just Name
                  "headline" → Just Headline
                  "description" → Just Description
                  _ → Nothing

errorDecoding ∷ ∀ a. Object (Array String) → String → Either DecodeError a
errorDecoding queryObj key = Left $ QueryDecodeError
      { values: []
      , message: "Could not decode parameter " <> key
      , key
      , queryObj
      }
