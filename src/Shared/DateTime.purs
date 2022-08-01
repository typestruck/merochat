module Shared.DateTime where

import Prelude


import Data.Show.Generic as DGRS
import Data.Array ((!!))
import Data.Date (Date)
import Data.Date as DD
import Data.DateTime (Date(..), DateTime(..), Time(..))
import Data.DateTime as DDT
import Data.DateTime.Instant as DDI
import Data.Enum (class BoundedEnum)
import Data.Enum as DE
import Data.Function.Uncurried as DFU
import Data.Int as DI
import Simple.JSON (class ReadForeign, class WriteForeign)
import Data.Int as DIN
import Data.Interval (DurationComponent(..))
import Payload.Client.QueryParams (class EncodeQueryParam)
import Payload.Server.QueryParams (class DecodeQueryParam, DecodeError(..))
import Data.List as DA
import Data.List as DL
import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Foreign.Object (Object)
import Foreign.Object as FO
import Droplet.Language (class FromValue, class ToValue)
import Data.Argonaut.Core as DAC
import Data.Number as DNM
import Data.Argonaut.Core as DAP
import Data.Newtype (class Newtype)
import Droplet.Language as DL
import Data.Newtype as DN
import Data.String (Pattern(..))
import Data.Either (Either(..))
import Data.String as DS
import Data.Time.Duration (Days(..))
import Data.Time.Duration as DTD
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect.Now as EN
import Effect.Unsafe as EU
import Foreign (Foreign, ForeignError(..), F)
import Foreign as F
import Shared.Unsafe as SU
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode as DAD
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic as DAEGR

foreign import time ∷ Number → String
foreign import dayOfTheWeek ∷ Number → String
foreign import fullDate ∷ Number → String

newtype DateTimeWrapper = DateTimeWrapper DateTime

ageFrom ∷ Maybe Date → Maybe Int
ageFrom = ageFrom' now
      where
      now = EU.unsafePerformEffect EN.nowDate

ageFrom' ∷ Date → Maybe Date → Maybe Int
ageFrom' now birthday = calculate <$> birthday
      where
      calculate b = DE.fromEnum (DD.year now) - DE.fromEnum (DD.year b) - if dayDiff (DateTime now zeroTime) < dayDiff (DateTime b zeroTime) then 1 else 0

--minimum age to sign up is 13
latestEligibleBirthday ∷ Effect Date
latestEligibleBirthday = do
      now ← EN.nowDate
      pure <<< SU.fromJust $ DD.adjust (Days $ negate (365.0 * 13.0)) now

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

dateToNumber ∷ ∀ n. Newtype n Date ⇒ n → Number
dateToNumber = DN.unwrap <<< DDI.unInstant <<< DDI.fromDate <<< DN.unwrap

dateTimeToNumber ∷ ∀ n. Newtype n DateTime ⇒ n → Number
dateTimeToNumber = DN.unwrap <<< DDI.unInstant <<< DDI.fromDateTime <<< DN.unwrap

formatISODate ∷ ∀ n. Newtype n Date ⇒ n → String
formatISODate dateWrapper = formatISODate' $ DN.unwrap dateWrapper

formatISODate' ∷ Date  → String
formatISODate' date = DA.intercalate "-" $ map (pad <<< show) [ DE.fromEnum $ DD.year date, DE.fromEnum $ DD.month date, DE.fromEnum $ DD.day date ]
      where
      pad value = case DS.length value of
            1 → "0" <> value
            _ → value

unformatISODate ∷ String → Maybe Date
unformatISODate value = do
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

unsafeNow ∷ DateTime
unsafeNow = EU.unsafePerformEffect EN.nowDateTime

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
      days = dayDiff dateTime

agoWithTime ∷ DateTime → String
agoWithTime dateTime =
      if days == 0 then
            timeString
      else
            ago dateTime <> " " <> timeString
      where
      days = dayDiff dateTime
      timeString = localDateTimeWith time dateTime

dayDiff ∷ DateTime → Int
dayDiff dateTime = days now - days dateTime
      where
      firstDay = DateTime (DD.canonicalDate (DD.year $ DDT.date now) (SU.toEnum 1) (SU.toEnum 1)) zeroTime
      now = unsafeNow
      days dt = DI.floor $ DN.unwrap (DDT.diff dt firstDay ∷ Days)

localDateTimeWith ∷ _ → DateTime → String
localDateTimeWith formatter = formatter <<< DN.unwrap <<< DDI.unInstant <<< DDI.fromDateTime


instance DecodeJson DateTimeWrapper where
      decodeJson = DM.maybe (Left $ DAD.TypeMismatch "couldn't parse epoch") (Right <<< DateTimeWrapper <<< DDI.toDateTime) <<< DAP.caseJsonNumber Nothing (DDI.instant <<< DTD.Milliseconds)

instance EncodeJson DateTimeWrapper where
      encodeJson = DAC.fromNumber <<< dateTimeToNumber

derive instance Ord DateTimeWrapper

derive instance Newtype DateTimeWrapper _

derive instance Generic DateTimeWrapper _

instance FromValue DateTimeWrapper where
      fromValue v = map DateTimeWrapper (DL.fromValue v ∷ Either String DateTime)

derive instance Eq DateTimeWrapper

instance WriteForeign DateTimeWrapper where
      writeImpl = F.unsafeToForeign <<< dateTimeToNumber

instance Show DateTimeWrapper where
      show = DGRS.genericShow

instance EncodeQueryParam DateTimeWrapper where
      encodeQueryParam = Just <<< show <<< dateTimeToNumber

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
