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
      | Custom String

derive instance Eq Badge

derive instance Ord Badge

derive instance Generic Badge _

instance DecodeJson Badge where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson Badge where
      encodeJson = DAEGR.genericEncodeJson

instance Bounded Badge where
      bottom = Admin
      top = Custom ""

instance BoundedEnum Badge where
      cardinality = Cardinality 1

      fromEnum = case _ of
            Admin → 0
            Contributor → 100
            Custom _ → 200

      toEnum = case _ of
            0 → Just Admin
            100 → Just Contributor
            200 → Just $ Custom ""
            _ → Nothing

instance Enum Badge where
      succ = case _ of
            Admin → Just Contributor
            Contributor → Just $ Custom ""
            Custom _ → Nothing

      pred = case _ of
            Admin → Nothing
            Contributor → Just Admin
            Custom _ → Just Contributor

instance Show Badge where
      show = case _ of
            Admin → "Admin"
            Contributor → "Contributor"
            Custom c → c

unshow :: String -> Badge
unshow = case _ of
            "Admin" → Admin
            "Contributor" → Contributor
            c → Custom c

instance ReadForeign Badge where
      readImpl f = unshow <$> F.readString f

instance WriteForeign Badge where
      writeImpl = F.unsafeToForeign <<< show

instance FromValue Badge where
      fromValue v = map unshow (DL.fromValue v ∷ Either String String)

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
      Custom c →
            { badge: b
            , text: c
            , description: "Special user badge: " <> c
            }

