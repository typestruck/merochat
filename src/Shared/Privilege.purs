module Shared.Privilege where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Array as DA
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic as DSG
import Foreign as F
import Shared.Unsafe as SU
import Simple.JSON (class ReadForeign, class WriteForeign)

data Privilege
      = StartChats
      | MoreTags
      | StartChatExperiments
      | SendLinks
      | SendImages

hasPrivilege ∷ ∀ r. Privilege → { privileges ∷ Array Privilege | r } → Boolean
hasPrivilege p { privileges } = DA.elem p privileges

derive instance Eq Privilege

derive instance Ord Privilege

derive instance Generic Privilege _

instance DecodeJson Privilege where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson Privilege where
      encodeJson = DAEGR.genericEncodeJson

instance Bounded Privilege where
      bottom = StartChats
      top = SendImages

instance BoundedEnum Privilege where
      cardinality = Cardinality 1

      fromEnum = case _ of
            StartChats → 0
            MoreTags → 1
            StartChatExperiments → 2
            SendLinks → 3
            SendImages → 4

      toEnum = case _ of
            0 → Just StartChats
            1 → Just MoreTags
            2 → Just StartChatExperiments
            3 → Just SendLinks
            4 → Just SendImages
            _ → Nothing

instance Enum Privilege where
      succ = case _ of
            StartChats → Just MoreTags
            MoreTags → Just StartChatExperiments
            StartChatExperiments → Just SendLinks
            SendLinks → Just SendImages
            SendImages → Nothing

      pred = case _ of
            StartChats → Nothing
            MoreTags → Just StartChats
            StartChatExperiments → Just MoreTags
            SendLinks → Just StartChatExperiments
            SendImages → Just SendLinks

instance Show Privilege where
      show = DSG.genericShow

instance ReadForeign Privilege where
      readImpl f = SU.fromJust <<< DE.toEnum <$> F.readInt f

instance WriteForeign Privilege where
      writeImpl = F.unsafeToForeign <<< DE.fromEnum
