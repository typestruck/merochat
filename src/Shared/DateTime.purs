module Shared.DateTime where

import Prelude

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
import Data.Int as DIN
import Data.Interval (DurationComponent(..))
import Data.List as DA
import Data.List as DL
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype as DN
import Data.String (Pattern(..))
import Data.String as DS
import Data.Time.Duration (Days(..))
import Data.Time.Duration as DTD
import Effect (Effect)
import Effect.Now as EN
import Effect.Unsafe as EU
import Foreign (Foreign, ForeignError(..), F)
import Foreign as F
import Shared.Unsafe as SU

foreign import time :: Number -> String
foreign import dayOfTheWeek :: Number -> String
foreign import fullDate :: Number -> String

ageFrom :: Maybe Date -> Maybe Int
ageFrom = ageFrom' now
      where now = EU.unsafePerformEffect EN.nowDate

ageFrom' :: Date -> Maybe Date -> Maybe Int
ageFrom' now birthday = calculate <$> birthday
      where calculate b = DE.fromEnum (DD.year now) - DE.fromEnum (DD.year b) - if dayDiff (DateTime now zeroTime) < dayDiff (DateTime b zeroTime) then 1 else 0

--minimum age to sign up is 13
latestEligibleBirthday :: Effect Date
latestEligibleBirthday = do
      now <- EN.nowDate
      pure <<< SU.fromJust $ DD.adjust (Days $ negate (365.0 * 13.0)) now

getYear :: forall t10. Newtype t10 Date => t10 -> Int
getYear = DE.fromEnum <<< DD.year <<< DN.unwrap

getMonth :: forall t22. Newtype t22 Date => t22 -> Int
getMonth = DE.fromEnum <<< DD.month <<< DN.unwrap

getDay :: forall t56. Newtype t56 Date => t56 -> Int
getDay = DE.fromEnum <<< DD.day <<< DN.unwrap

epoch :: DateTime
epoch = DateTime (DD.canonicalDate (SU.toEnum 3000) (SU.toEnum 1) (SU.toEnum 1)) zeroTime

zeroTime :: Time
zeroTime = Time (SU.toEnum 0) (SU.toEnum 0) (SU.toEnum 0) (SU.toEnum 0)

readDate :: Foreign -> F DateTime
readDate foreignDateTime = do
      milliseconds <- F.readNumber foreignDateTime
      case DDI.instant $ DTD.Milliseconds milliseconds of
            Nothing -> F.fail $ ForeignError "could not parse datetime"
            Just instant -> pure $ DDI.toDateTime instant

dateToNumber :: forall n. Newtype n Date => n -> Number
dateToNumber = DN.unwrap <<< DDI.unInstant <<< DDI.fromDate <<< DN.unwrap

dateTimeToNumber :: forall n. Newtype n DateTime => n -> Number
dateTimeToNumber = DN.unwrap <<< DDI.unInstant <<< DDI.fromDateTime <<< DN.unwrap

formatISODate :: forall n. Newtype n Date => n -> String
formatISODate dateWrapper = DA.intercalate "-" $ map (pad <<< show) [DE.fromEnum $ DD.year date, DE.fromEnum $ DD.month date, DE.fromEnum $ DD.day date]
      where date = DN.unwrap dateWrapper
            pad value = case DS.length value of
                  1 -> "0" <> value
                  _ -> value

unformatISODate :: String -> Maybe Date
unformatISODate value = do
      year <- parseUnit 0
      month <- parseUnit 1
      day <- parseUnit 2
      pure $ DD.canonicalDate year month day
      where split = DS.split (Pattern "-") value
            parseUnit :: forall v. BoundedEnum v => Int -> Maybe v
            parseUnit n = do
                  raw <- split !! n
                  integered <- DI.fromString raw
                  DE.toEnum integered

unsafeNow :: DateTime
unsafeNow = EU.unsafePerformEffect EN.nowDateTime

--same day
--    hh:mm
--yesterday
--    yesterday
--same week
--    day of week
--otherwise
--    mdy
ago :: DateTime -> String
ago dateTime =
      if days == 0 then
            localDateTimeWith time dateTime
       else if days == 1 then
            "Yesterday"
       else if days >= 2 && days <= 7 then
            localDateTimeWith dayOfTheWeek dateTime
       else
            localDateTimeWith fullDate dateTime
      where days = dayDiff dateTime

agoWithTime :: DateTime -> String
agoWithTime dateTime =
      if days == 0 then
            timeString
       else
            ago dateTime <> " " <> timeString
      where days = dayDiff dateTime
            timeString = localDateTimeWith time dateTime

dayDiff :: DateTime -> Int
dayDiff dateTime = days now - days dateTime
      where firstDay = DateTime (DD.canonicalDate (DD.year $ DDT.date now) (SU.toEnum 1) (SU.toEnum 1)) zeroTime
            now = unsafeNow
            days dt = DI.floor $ DN.unwrap (DDT.diff dt firstDay :: Days)

localDateTimeWith :: _ -> DateTime -> String
localDateTimeWith formatter = formatter <<< DN.unwrap <<< DDI.unInstant <<< DDI.fromDateTime