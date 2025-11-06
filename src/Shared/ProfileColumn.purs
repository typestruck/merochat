module Shared.ProfileColumn where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Either (Either)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic as DSG
import Droplet.Language (class FromValue, class ToValue)
import Droplet.Language as DL
import Foreign as F
import Shared.Unsafe as SU
import Simple.JSON (class ReadForeign, class WriteForeign)

data ProfileColumn
      = Name
      | Headline
      | Description
      | Avatar
      | Birthday
      | ChatBackground
      | Gender
      | Country
      | Languages
      | Tags

displayColumn ∷ ProfileColumn → String
displayColumn = case _ of
      Name → "Display name"
      Headline → "Headline"
      Description → "Description"
      Avatar → "Profile picture"
      Birthday → "Age"
      Gender → "Gender"
      Country → "Country"
      Languages → "Language"
      Tags → "Tags"
      ChatBackground -> "Chat background"

instance Show ProfileColumn where
      show = DSG.genericShow

derive instance Generic ProfileColumn _

derive instance Eq ProfileColumn

derive instance Ord ProfileColumn

instance Bounded ProfileColumn where
      bottom = Name
      top = Birthday

instance BoundedEnum ProfileColumn where
      cardinality = Cardinality 1

      fromEnum = case _ of
            Name → 0
            Headline → 1
            Description → 2
            Avatar → 3
            Birthday → 4
            Gender → 5
            Country → 6
            Languages → 7
            Tags → 8
            ChatBackground -> 9

      toEnum = case _ of
            0 → Just Name
            1 → Just Headline
            2 → Just Description
            3 → Just Avatar
            4 → Just Birthday
            5 → Just Gender
            6 → Just Country
            7 → Just Languages
            8 → Just Tags
            _ → Nothing

instance Enum ProfileColumn where
      succ = case _ of
            Name → Just Headline
            Headline → Just Description
            Description → Just Avatar
            Avatar → Just Birthday
            Birthday → Just Gender
            Gender → Just Country
            Country → Just Languages
            Languages → Just Tags
            Tags → Just ChatBackground
            ChatBackground ->  Nothing

      pred = case _ of
            Name → Nothing
            Headline → Just Name
            Description → Just Headline
            Avatar → Just Description
            Birthday → Just Avatar
            Gender → Just Birthday
            Country → Just Gender
            Languages → Just Country
            Tags → Just Languages
            ChatBackground -> Just Tags

instance FromValue ProfileColumn where
      fromValue v = map (SU.fromJust <<< DE.toEnum) (DL.fromValue v ∷ Either String Int)

instance ToValue ProfileColumn where
      toValue = F.unsafeToForeign <<< DE.fromEnum

instance EncodeJson ProfileColumn where
      encodeJson = DAEGR.genericEncodeJson

instance DecodeJson ProfileColumn where
      decodeJson = DADGR.genericDecodeJson

instance ReadForeign ProfileColumn where
      readImpl f = SU.fromJust <<< DE.toEnum <$> F.readInt f

instance WriteForeign ProfileColumn where
      writeImpl = F.unsafeToForeign <<< DE.fromEnum
