module Shared.Availability where

import Shared.DateTime (DateTimeWrapper(..), dateTimeToNumber)
import Prelude
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Generic.Rep (class Generic)
import Data.Time.Duration as DTD
import Foreign as F
import Data.DateTime.Instant as DDI
import Shared.DateTime as SDT
import Shared.Unsafe as SU
import Simple.JSON (class ReadForeign, class WriteForeign)

data Availability
      = None -- no data or private online status Online
      | LastSeen DateTimeWrapper
      | Online
      | Unavailable -- blocked/deleted/private profile

derive instance Eq Availability

derive instance Ord Availability

derive instance Generic Availability _

instance DecodeJson Availability where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson Availability where
      encodeJson = DAEGR.genericEncodeJson

instance Show Availability where
      show = case _ of
            Online → "Online"
            LastSeen (DateTimeWrapper dt) → "Last seen " <> SDT.ago dt
            Unavailable → "Unavailable"
            None → ""

instance ReadForeign Availability where
      readImpl f = availabilityFromWire <$> F.readNumber f

instance WriteForeign Availability where
      writeImpl = F.unsafeToForeign <<< availabilityToWire

availabilityToWire ∷ Availability → Number
availabilityToWire = case _ of
      Online → 0.0
      LastSeen dt → dateTimeToNumber dt
      Unavailable → 2.0
      None → 3.0

availabilityFromWire ∷ Number → Availability
availabilityFromWire = case _ of
      0.0 → Online
      2.0 → Unavailable
      3.0 → None
      ms → LastSeen <<< DateTimeWrapper <<< DDI.toDateTime <<< SU.fromJust <<< DDI.instant $ DTD.Milliseconds ms
