module Shared.SuggestionsFrom where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Show.Generic as DGS
import Droplet.Language (class FromValue, class ToValue)
import Droplet.Language as DL
import Foreign as F
import Foreign.Object as FO
import Payload.Client.QueryParams (class EncodeQueryParam)
import Payload.Server.QueryParams (class DecodeQueryParam, DecodeError(..))

data SuggestionsFrom
      = ThisWeek
      | LastTwoWeeks
      | LastMonth
      | All
      | OnlineOnly
      | ContactsOnly
      | FavoritesOnly

derive instance Eq SuggestionsFrom
derive instance Ord SuggestionsFrom
derive instance Generic SuggestionsFrom _

instance Bounded SuggestionsFrom where
      bottom = ThisWeek
      top = FavoritesOnly

instance BoundedEnum SuggestionsFrom where
      cardinality = Cardinality 1
      fromEnum = case _ of
            OnlineOnly → 0
            ThisWeek → 1
            LastTwoWeeks → 2
            LastMonth → 3
            All → 4
            ContactsOnly → 5
            FavoritesOnly → 6
      toEnum = case _ of
            0 → Just OnlineOnly
            1 → Just ThisWeek
            2 → Just LastTwoWeeks
            3 → Just LastMonth
            4 → Just All
            5 → Just ContactsOnly
            6 → Just FavoritesOnly
            _ → Nothing

instance Enum SuggestionsFrom where
      succ = case _ of
            OnlineOnly → Just ThisWeek
            ThisWeek → Just LastTwoWeeks
            LastTwoWeeks → Just LastMonth
            LastMonth → Just All
            All → Just ContactsOnly
            ContactsOnly → Just FavoritesOnly
            FavoritesOnly → Nothing
      pred = case _ of
            OnlineOnly → Nothing
            ThisWeek → Just OnlineOnly
            LastTwoWeeks → Just ThisWeek
            LastMonth → Just LastTwoWeeks
            All → Just LastMonth
            ContactsOnly → Just All
            FavoritesOnly → Just ContactsOnly

instance DecodeJson SuggestionsFrom where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson SuggestionsFrom where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeQueryParam SuggestionsFrom where
      encodeQueryParam = Just <<< show <<< DE.fromEnum

instance ToValue SuggestionsFrom where
      toValue = F.unsafeToForeign <<< DE.fromEnum

instance Show SuggestionsFrom where
      show = DGS.genericShow

instance FromValue SuggestionsFrom where
      fromValue value = case DL.fromValue value of
            Left error → Left error
            Right number → case DE.toEnum number of
                  Nothing → Left "Invalid SuggestionsFrom value"
                  Just result → Right result

instance DecodeQueryParam SuggestionsFrom where
      decodeQueryParam query key =
            case FO.lookup key query of
                  Nothing → Left $ QueryParamNotFound { key, queryObj: query }
                  Just [ value ] → case DI.fromString value >>= DE.toEnum of
                        Nothing → Left $ QueryDecodeError { values: [], message: "Could not decode parameter " <> key, key, queryObj: query }
                        Just value' → Right value'
                  _ → Left $ QueryDecodeError { values: [], message: "Could not decode parameter " <> key, key, queryObj: query }
