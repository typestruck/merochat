module Shared.Badge where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Either (Either)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic as DSG
import Droplet.Language (class FromValue, class ToValue)
import Droplet.Language as DL
import Foreign as F
import Shared.Unsafe as SU
import Simple.JSON (class ReadForeign, class WriteForeign)

data Badge
      = Admin
      | Contributor

derive instance Eq Badge

derive instance Ord Badge

derive instance Generic Badge _

instance DecodeJson Badge where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson Badge where
      encodeJson = DAEGR.genericEncodeJson

instance Bounded Badge where
      bottom = Admin
      top = Contributor

instance BoundedEnum Badge where
      cardinality = Cardinality 1

      fromEnum = case _ of
            Admin → 0
            Contributor → 100

      toEnum = case _ of
            0 → Just Admin
            100 → Just Contributor
            _ → Nothing

instance Enum Badge where
      succ = case _ of
            Admin → Just Contributor
            Contributor → Nothing

      pred = case _ of
            Admin → Nothing
            Contributor → Just Admin

instance Show Badge where
      show = DSG.genericShow

instance ReadForeign Badge where
      readImpl f = SU.fromJust <<< DE.toEnum <$> F.readInt f

instance WriteForeign Badge where
      writeImpl = F.unsafeToForeign <<< DE.fromEnum

instance ToValue Badge where
      toValue = F.unsafeToForeign <<< DE.fromEnum

instance FromValue Badge where
      fromValue v = map (SU.fromJust <<< DE.toEnum) (DL.fromValue v ∷ Either String Int)

type BadgeUser =
      { badge ∷ Badge
      , text ∷ String
      , description ∷ String
      }

badgeFor ∷ Badge → BadgeUser
badgeFor b = case b of
      Admin →
            { badge: b
            , text: "Admin"
            , description: "This user is a MeroChat admin"
            }
      Contributor →
            { badge: b
            , text: "Contributor"
            , description: "This user has contributed to MeroChat"
            }

