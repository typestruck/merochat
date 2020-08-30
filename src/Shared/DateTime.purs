module Shared.DateTime where

import Prelude

import Data.Date as DD
import Data.DateTime (Date, DateTime(..), Time(..))
import Data.DateTime as DT
import Data.Either (Either)
import Data.Enum as DE
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.Formatter.DateTime as DFD
import Data.Int as DIN
import Data.List (List)
import Data.List as DL
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Newtype as DN
import Data.Time.Duration (Days(..))
import Effect (Effect)
import Effect.Now as EN
import Effect.Unsafe as EU
import Shared.Unsafe as SU

--this will yield a wrong result in some cases, but I guess it is fair for a ASL field
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

dateFormat :: List FormatterCommand
dateFormat = DL.singleton UnixTimestamp

formatDateTime :: DateTime -> String
formatDateTime = DFD.format dateFormat

unformatDateTime :: String -> Either String DateTime
unformatDateTime = DFD.unformat dateFormat

formatDate :: Date -> String
formatDate date = DFD.format dateFormat $ DateTime date zeroTime

unformatDate :: String -> Either String Date
unformatDate value = DT.date <$> DFD.unformat dateFormat value

epoch :: DateTime
epoch = DateTime (DD.canonicalDate (SU.toEnum 1970) (SU.toEnum 1) (SU.toEnum 1)) zeroTime

zeroTime :: Time
zeroTime = Time (SU.toEnum 0) (SU.toEnum 0) (SU.toEnum 0) (SU.toEnum 0)