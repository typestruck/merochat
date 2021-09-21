module Shared.User where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Either (Either)
import Data.Either as DET
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.String.Read (class Read)
import Data.String.Read as DSR
import Droplet.Language (class FromValue, class ToValue)
import Droplet.Language as DL
import Foreign as F
import Payload.Client.EncodeBody (class EncodeBody)
import Payload.ContentType (class HasContentType, json)
import Payload.Server.DecodeBody (class DecodeBody)
import Shared.Unsafe as SU
import Simple.JSON (class ReadForeign, class WriteForeign)

type BasicUser fields =
      ( id ∷ Int
      , name ∷ String
      , headline ∷ String
      , description ∷ String
      , avatar ∷ Maybe String
      , tags ∷ Array String
      , karma ∷ Int
      , karmaPosition ∷ Int
      | fields
      )

type IU =
      ( BasicUser
              ( gender ∷ Maybe String
              , country ∷ Maybe String
              , languages ∷ Array String
              , age ∷ Maybe Int
              )
      )

data Gender
      = Female
      | Male
      | NonBinary
      | Other

data ProfileVisibility
      = Everyone
      | Contacts
      | Nobody
      | TemporarilyBanned -- user is deleted when banned for good

derive instance Eq ProfileVisibility
derive instance Eq Gender

instance DecodeJson Gender where
      decodeJson = DADGR.genericDecodeJson
instance DecodeJson ProfileVisibility where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson Gender where
      encodeJson = DAEGR.genericEncodeJson
instance EncodeJson ProfileVisibility where
      encodeJson = DAEGR.genericEncodeJson

instance Show Gender where
      show Female = "Female"
      show Male = "Male"
      show NonBinary = "Non binary"
      show Other = "Other"

instance ReadForeign Gender where
      readImpl foreignGender = SU.fromJust <<< DSR.read <$> F.readString foreignGender

derive instance Generic Gender _
derive instance Generic ProfileVisibility _

instance WriteForeign Gender where
      writeImpl gender = F.unsafeToForeign $ show gender

instance ToValue Gender where
      toValue = F.unsafeToForeign <<< DE.fromEnum

derive instance Ord Gender
derive instance Ord ProfileVisibility

--refactor: should just use Enum (have to fix read/writeforeign instances for gender)
instance Read Gender where
      read input =
            case DS.toLower $ DS.trim input of
                  "female" → Just Female
                  "male" → Just Male
                  "non binary" → Just NonBinary
                  "other" → Just Other
                  _ → Nothing

instance Bounded Gender where
      bottom = Female
      top = Other

instance BoundedEnum Gender where
      cardinality = Cardinality 1

      fromEnum = case _ of
            Female → 0
            Male → 1
            NonBinary → 2
            Other → 3

      toEnum = case _ of
            0 → Just Female
            1 → Just Male
            2 → Just NonBinary
            3 → Just Other
            _ → Nothing

instance Enum Gender where
      succ = case _ of
            Female → Just Male
            Male → Just NonBinary
            NonBinary → Just Other
            Other → Nothing

      pred = case _ of
            Female → Nothing
            Male → Just Female
            NonBinary → Just Male
            Other → Just NonBinary


instance Bounded ProfileVisibility where
      bottom = Everyone
      top = TemporarilyBanned

instance BoundedEnum ProfileVisibility where
      cardinality = Cardinality 1

      fromEnum = case _ of
            Everyone → 0
            Contacts → 1
            Nobody → 2
            TemporarilyBanned → 3

      toEnum = case _ of
            0 → Just Everyone
            1 → Just Contacts
            2 → Just Nobody
            3 → Just TemporarilyBanned
            _ → Nothing

instance Enum ProfileVisibility where
      succ = case _ of
            Everyone → Just Contacts
            Contacts → Just Nobody
            Nobody → Just TemporarilyBanned
            TemporarilyBanned → Nothing

      pred = case _ of
            Everyone → Nothing
            Contacts → Just Everyone
            Nobody → Just Contacts
            TemporarilyBanned → Just Nobody

instance FromValue Gender where
      fromValue v = map (SU.fromJust <<< DE.toEnum) (DL.fromValue v ∷ Either String Int)

instance FromValue ProfileVisibility where
      fromValue v = map (SU.fromJust <<< DE.toEnum) (DL.fromValue v ∷ Either String Int)

instance ToValue ProfileVisibility where
      toValue = F.unsafeToForeign <<< DE.fromEnum

instance HasContentType ProfileVisibility where
      getContentType = const json

instance EncodeBody ProfileVisibility where
      encodeBody = show <<< DE.fromEnum

instance DecodeBody ProfileVisibility where
      decodeBody s = DET.note ("Could not decode body " <> s) (DE.toEnum =<< DI.fromString s)