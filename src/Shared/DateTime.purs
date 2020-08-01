module Shared.DateTime where

import Prelude

import Data.Date as DD
import Data.DateTime (Date)
import Data.Int as DIN
import Data.Maybe (Maybe)
import Data.Time.Duration (Days(..))
import Effect (Effect)
import Data.Enum as DE
import Shared.Types
import Effect.Now as EN
import Effect.Unsafe as EU
import Shared.Unsafe as SU

--this will yield a wrong result in some cases, but I guess it is fair for a ASL field
ageFrom :: Maybe Date -> Maybe Int
ageFrom birthday = fromDays <<< DD.diff now <$> birthday
        where   now = EU.unsafePerformEffect EN.nowDate

                fromDays (Days d) = DIN.floor (d / 365.0)

getMinimumYear :: Effect Int
getMinimumYear = do
        now <- EN.nowDate
        pure $ SU.fromJust do
                date <- DD.adjust (Days (negate (365.0 * 13.0))) now
                pure <<< DE.fromEnum $ DD.year date

getYear :: MDate -> Int
getYear (MDate date) = DE.fromEnum $ DD.year date

getMonth :: MDate -> Int
getMonth (MDate date) = DE.fromEnum $ DD.month date

getDay :: MDate -> Int
getDay (MDate date) = DE.fromEnum $ DD.day date