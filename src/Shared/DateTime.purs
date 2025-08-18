module Shared.DateTime where

import Prelude

import Data.Argonaut.Core as DAC
import Data.Argonaut.Core as DAP
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode as DAD
import Data.Argonaut.Encode (class EncodeJson)
import Data.Array ((!!))
import Data.Date as DD
import Data.DateTime (Date, DateTime(..), Time(..))
import Data.DateTime as DT
import Data.DateTime.Instant as DDI
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum)
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Int as DI
import Data.List as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype (class Newtype)
import Data.Newtype as DN
import Data.Number as DNM
import Data.Show.Generic as DGRS
import Data.String (Pattern(..))
import Data.String as DS
import Data.Time as DTM
import Data.Time.Duration (class Duration, Days(..), Minutes(..))
import Data.Time.Duration as DTD
import Debug (spy)
import Droplet.Language (class FromValue, class ToValue)
import Droplet.Language as DL
import Effect (Effect)
import Effect.Now as EN
import Effect.Unsafe as EU
import Flame.Html.Attribute (d)
import Foreign (Foreign, ForeignError(..), F)
import Foreign as F
import Foreign.Object (Object)
import Foreign.Object as FO
import Payload.Client.QueryParams (class EncodeQueryParam)
import Payload.Server.QueryParams (class DecodeQueryParam, DecodeError(..))
import Shared.Unsafe as SU
import Simple.JSON (class ReadForeign, class WriteForeign)

foreign import time ∷ Number → String
foreign import dayOfTheWeek ∷ Number → String
foreign import fullDate ∷ Number → String

newtype DateWrapper = DateWrapper Date

newtype DateTimeWrapper = DateTimeWrapper DateTime

ageFrom ∷ DateWrapper → Int
ageFrom (DateWrapper dt) = ageFrom' now dt
      where
      now = EU.unsafePerformEffect EN.nowDate

ageFrom' ∷ Date → Date → Int
ageFrom' now birthday = DE.fromEnum currentYear - DE.fromEnum birthdayYear - if DateTime now zeroTime < birthdayThisYear then 1 else 0
      where
      currentYear = DD.year now
      birthdayYear = DD.year birthday
      birthdayMonth = DD.month birthday
      birthdayDay = DD.day birthday
      birthdayThisYear = DateTime (DD.canonicalDate currentYear birthdayMonth birthdayDay) zeroTime

--minimum age to sign up is 18
latestEligibleBirthday ∷ Effect Date
latestEligibleBirthday = do
      now ← EN.nowDate
      pure <<< SU.fromJust $ DD.adjust (Days $ negate (365.0 * 18.0)) now

getYear ∷ ∀ t10. Newtype t10 Date ⇒ t10 → Int
getYear = DE.fromEnum <<< DD.year <<< DN.unwrap

getMonth ∷ ∀ t22. Newtype t22 Date ⇒ t22 → Int
getMonth = DE.fromEnum <<< DD.month <<< DN.unwrap

getDay ∷ ∀ t56. Newtype t56 Date ⇒ t56 → Int
getDay = DE.fromEnum <<< DD.day <<< DN.unwrap

epoch ∷ DateTime
epoch = DateTime (DD.canonicalDate (SU.toEnum 3000) (SU.toEnum 1) (SU.toEnum 1)) zeroTime

zeroTime ∷ Time
zeroTime = Time (SU.toEnum 0) (SU.toEnum 0) (SU.toEnum 0) (SU.toEnum 0)

readDate ∷ Foreign → F DateTime
readDate foreignDateTime = do
      milliseconds ← F.readNumber foreignDateTime
      case DDI.instant $ DTD.Milliseconds milliseconds of
            Nothing → F.fail $ ForeignError "could not parse datetime"
            Just instant → pure $ DDI.toDateTime instant

dateToNumber ∷ DateWrapper → Number
dateToNumber = DN.unwrap <<< DDI.unInstant <<< DDI.fromDate <<< DN.unwrap

dateTimeToNumber ∷ DateTimeWrapper → Number
dateTimeToNumber = DN.unwrap <<< DDI.unInstant <<< DDI.fromDateTime <<< DN.unwrap

formatIsoDate ∷ DateWrapper → String
formatIsoDate dateWrapper = formatIsoDate' $ DN.unwrap dateWrapper

formatIsoDate' ∷ Date → String
formatIsoDate' date = DA.intercalate "-" $ map (pad <<< show) [ DE.fromEnum $ DD.year date, DE.fromEnum $ DD.month date, DE.fromEnum $ DD.day date ]
      where
      pad value = case DS.length value of
            1 → "0" <> value
            _ → value

unformatIsoDate ∷ String → Maybe Date
unformatIsoDate value = do
      year ← parseUnit 0
      month ← parseUnit 1
      day ← parseUnit 2
      pure $ DD.canonicalDate year month day
      where
      split = DS.split (Pattern "-") value

      parseUnit ∷ ∀ v. BoundedEnum v ⇒ Int → Maybe v
      parseUnit n = do
            raw ← split !! n
            integered ← DI.fromString raw
            DE.toEnum integered

--same day
--    hh:mm
--yesterday
--    yesterday
--same week
--    day of week
--otherwise
--    mdy
ago ∷ DateTime → String
ago dateTime =
      if days == 0 then
            localDateTimeWith time dateTime
      else if days == 1 then
            "Yesterday"
      else if days >= 2 && days <= 7 then
            localDateTimeWith dayOfTheWeek dateTime
      else
            localDateTimeWith fullDate dateTime
      where
      days = daysDiff dateTime

agoWithTime ∷ DateTime → String
agoWithTime dateTime =
      if days == 0 then
            timeString
      else
            ago dateTime <> " " <> timeString
      where
      days = daysDiff dateTime
      timeString = localDateTimeWith time dateTime

daysDiff ∷ DateTime → Int
daysDiff dt = daysInYear now - daysInYear dateTime
      where
      unsafeNow = EU.unsafePerformEffect EN.nowDateTime
      localTime = SU.fromJust <<< DT.adjust offset
      daysInYear dtp = DI.floor $ DN.unwrap (DT.diff dtp firstDay ∷ Days)
      firstDay = DateTime (DD.canonicalDate (DD.year $ DT.date now) (SU.toEnum 1) (SU.toEnum 1)) zeroTime
      offset = DTD.negateDuration $ EU.unsafePerformEffect EN.getTimezoneOffset
      now = localTime unsafeNow
      dateTime = localTime dt

unsafeAdjustFromNow ∷ ∀ d. Duration d ⇒ d → DateTime
unsafeAdjustFromNow duration = SU.fromJust $ DT.adjust duration unsafeNow
      where
      unsafeNow = EU.unsafePerformEffect EN.nowDateTime

localDateTimeWith ∷ (Number → String) → DateTime → String
localDateTimeWith formatter = formatter <<< DN.unwrap <<< DDI.unInstant <<< DDI.fromDateTime

instance DecodeJson DateWrapper where
      decodeJson = DM.maybe (Left $ DAD.TypeMismatch "couldn't parse epoch") (Right <<< DateWrapper <<< DT.date <<< DDI.toDateTime) <<< DAP.caseJsonNumber (Nothing) (DDI.instant <<< DTD.Milliseconds)

instance DecodeJson DateTimeWrapper where
      decodeJson = DM.maybe (Left $ DAD.TypeMismatch "couldn't parse epoch") (Right <<< DateTimeWrapper <<< DDI.toDateTime) <<< DAP.caseJsonNumber Nothing (DDI.instant <<< DTD.Milliseconds)

instance EncodeJson DateWrapper where
      encodeJson = DAC.fromNumber <<< dateToNumber

instance EncodeJson DateTimeWrapper where
      encodeJson = DAC.fromNumber <<< dateTimeToNumber

derive instance Ord DateTimeWrapper

derive instance Eq DateWrapper

derive instance Newtype DateTimeWrapper _

derive instance Generic DateWrapper _

derive instance Newtype DateWrapper _

derive instance Generic DateTimeWrapper _

instance FromValue DateTimeWrapper where
      fromValue v = map DateTimeWrapper (DL.fromValue v ∷ Either String DateTime)

derive instance Eq DateTimeWrapper

instance WriteForeign DateWrapper where
      writeImpl = F.unsafeToForeign <<< dateToNumber

instance WriteForeign DateTimeWrapper where
      writeImpl = F.unsafeToForeign <<< dateTimeToNumber

instance Show DateWrapper where
      show = DGRS.genericShow

instance Show DateTimeWrapper where
      show = DGRS.genericShow

instance FromValue DateWrapper where
      fromValue v = map DateWrapper (DL.fromValue v ∷ Either String Date)

instance ToValue DateWrapper where
      toValue = F.unsafeToForeign <<< formatIsoDate

instance EncodeQueryParam DateTimeWrapper where
      encodeQueryParam = Just <<< show <<< dateTimeToNumber

instance ReadForeign DateWrapper where
      readImpl foreignDate = DateWrapper <<< DT.date <<< DDI.toDateTime <<< SU.fromJust <<< DDI.instant <<< DTD.Milliseconds <$> F.readNumber foreignDate

instance ReadForeign DateTimeWrapper where
      readImpl foreignDateTime = DateTimeWrapper <<< DDI.toDateTime <<< SU.fromJust <<< DDI.instant <<< DTD.Milliseconds <$> F.readNumber foreignDateTime

instance DecodeQueryParam DateTimeWrapper where
      decodeQueryParam query key =
            case FO.lookup key query of
                  Nothing → Left $ QueryParamNotFound { key, queryObj: query }
                  Just [ value ] → DM.maybe (errorDecoding query key) (Right <<< DateTimeWrapper <<< DDI.toDateTime) (DDI.instant <<< DTD.Milliseconds =<< DNM.fromString value)
                  _ → errorDecoding query key

errorDecoding ∷ ∀ a. Object (Array String) → String → Either DecodeError a
errorDecoding queryObj key = Left $ QueryDecodeError
      { values: []
      , message: "Could not decode parameter " <> key
      , key
      , queryObj
      }
