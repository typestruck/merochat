module Shared.User where

import Prelude
import Shared.DateTime

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic as DAEGR
import Data.DateTime as DDT
import Data.Either (Either)
import Data.Either as DET
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Show.Generic as DSG
import Data.String as DS
import Data.String.Read (class Read)
import Data.String.Read as DSR
import Data.Time.Duration (Days(..))
import Data.Time.Duration as DTD
import Droplet.Language (class FromValue, class ToValue)
import Droplet.Language as DL
import Foreign as F
import Payload.Client.EncodeBody (class EncodeBody)
import Payload.ContentType (class HasContentType, json)
import Payload.Server.DecodeBody (class DecodeBody)
import Shared.DateTime as SDT
import Shared.Unsafe as SU
import Simple.JSON (class ReadForeign, class WriteForeign)
import Unsafe.Coerce as UC

type BasicUser fields =
      ( id ∷ Int
      , name ∷ String
      , headline ∷ String
      , description ∷ String
      , availability ∷ Availability
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
              , readReceipts ∷ Boolean
              , typingStatus ∷ Boolean
              , profileVisibility ∷ ProfileVisibility
              , onlineStatus ∷ Boolean
              , temporary :: Boolean
              , messageTimestamps ∷ Boolean
             , joined :: DateTimeWrapper
              , completedTutorial ∷ Boolean
              )
      )

data Availability
      = Online
      | LastSeen DateTimeWrapper
      | Unavailable -- blocked/deleted/private profile
      | None -- no data or private online status

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
derive instance Eq Availability

instance DecodeJson Availability where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson Gender where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson ProfileVisibility where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson Gender where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson ProfileVisibility where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson Availability where
      encodeJson = DAEGR.genericEncodeJson

instance Show Gender where
      show = case _ of
            Female → "Female"
            Male → "Male"
            NonBinary → "Non binary"
            Other → "Other"

instance Show ProfileVisibility where
      show = DSG.genericShow

instance Show Availability where
      show = case _ of
            Online → "Online"
            LastSeen (DateTimeWrapper dt) → "Last seen " <> SDT.ago dt
            Unavailable → "Unavailable"
            None → ""

instance ReadForeign Gender where
      readImpl foreignGender = SU.fromJust <<< DSR.read <$> F.readString foreignGender

instance ReadForeign ProfileVisibility where
      readImpl f = SU.fromJust <<< DE.toEnum <$> F.readInt f

instance ReadForeign Availability where
      readImpl f = SU.fromJust <<< DE.toEnum <$> F.readInt f

derive instance Generic Gender _
derive instance Generic Availability _
derive instance Generic ProfileVisibility _

instance WriteForeign Gender where
      writeImpl gender = F.unsafeToForeign $ show gender

instance WriteForeign ProfileVisibility where
      writeImpl = F.unsafeToForeign <<< DE.fromEnum

instance WriteForeign Availability where
      writeImpl = F.unsafeToForeign <<< DE.fromEnum

instance ToValue Gender where
      toValue = F.unsafeToForeign <<< DE.fromEnum

derive instance Ord Gender
derive instance Ord Availability
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

instance Bounded Availability where
      bottom = Online
      top = None

instance BoundedEnum Availability where
      cardinality = Cardinality 1

      fromEnum = case _ of
            Online → 0
            LastSeen _ → 1
            Unavailable → 2
            None → 3

      toEnum = case _ of
            0 → Just Online
            1 → Just (LastSeen $ UC.unsafeCoerce 3)
            2 → Just Unavailable
            3 → Just None
            _ → Nothing

instance Enum Availability where
      succ = case _ of
            Online → Just (LastSeen $ UC.unsafeCoerce 3)
            LastSeen _ → Just Unavailable
            Unavailable → Just None
            None → Nothing

      pred = case _ of
            Online → Nothing
            LastSeen _ → Just Online
            Unavailable → Just (LastSeen $ UC.unsafeCoerce 3)
            None → Just Unavailable

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

temporaryAccountDuration :: Days
temporaryAccountDuration = Days 3.0

temporaryUserExpiration :: DateTimeWrapper -> Days
temporaryUserExpiration (DateTimeWrapper dt) = DDT.diff dt (SU.fromJust $ DDT.adjust (DTD.negateDuration temporaryAccountDuration) SDT.unsafeNow)