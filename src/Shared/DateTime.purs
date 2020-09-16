module Shared.DateTime where

import Prelude

import Data.Date as DD
import Data.DateTime (Date, DateTime(..), Time(..))
import Data.DateTime as DDT
import Data.DateTime.Instant as DDI
import Data.Enum as DE
import Data.Function.Uncurried (Fn6)
import Data.Function.Uncurried as DFU
import Data.Int as DI
import Data.Int as DIN
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype as DN
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

--REFACTOR: take now as parameter
ageFrom :: Maybe Date -> Maybe Int
ageFrom birthday = fromDays <<< DD.diff now <$> birthday
      where now = EU.unsafePerformEffect EN.nowDate
            fromDays (Days d) = DIN.floor (d / 365.0)

--minimum age to sign up is 13
getEarliestYear :: Effect Int
getEarliestYear = do
      now <- EN.nowDate
      pure <<< DE.fromEnum <<< DD.year <<< SU.fromJust $ DD.adjust (Days $ negate (365.0 * 13.0)) now

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

--same day
--    hh:mm
--yesterday
--    yesterday, HH:mm
--same week
--    day of week, HH:mm
--otherwise
--    mdy hh:mm
ago :: DateTime -> String
ago dateTime =
      let days = DI.ceil $ DN.unwrap (DDT.diff now dateTime :: Days)
      in
            if days == 0 then
                  localDateTimeWith time dateTime
             else if days == 1 then
                  "Yesterday " <> localDateTimeWith time dateTime
             else if days >= 2 && days <= 7 then
                  localDateTimeWith  dayOfTheWeek dateTime <> " " <> localDateTimeWith time dateTime
             else
                  localDateTimeWith fullDate dateTime
      where now :: DateTime
            now = EU.unsafePerformEffect EN.nowDateTime

localDateTimeWith :: _ -> DateTime -> String
localDateTimeWith formatter = formatter <<< DN.unwrap <<< DDI.unInstant <<< DDI.fromDateTime