module Shared.Availability where

import Shared.DateTime
import Prelude
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
import Data.DateTime.Instant as DDI
import Shared.DateTime as SDT
import Shared.Privilege (Privilege)
import Shared.Unsafe as SU
import Simple.JSON (class ReadForeign, class WriteForeign)

data Availability
      = Online
      | LastSeen DateTimeWrapper
      | Unavailable -- blocked/deleted/private profile
      | None -- no data or private online status

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

availabilityToWire = case _ of
      Online → 0.0
      LastSeen dt → dateTimeToNumber dt
      Unavailable → 2.0
      None → 3.0

availabilityFromWire = case _ of
      0.0 → Online
      2.0 → Unavailable
      3.0 → None
      ms → LastSeen <<< DateTimeWrapper <<< DDI.toDateTime <<< SU.fromJust <<< DDI.instant $ DTD.Milliseconds ms
