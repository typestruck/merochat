module Shared.Privilege where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Array as DA
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

data Privilege
      = ReceiveChats
      | StartChats
      | StartChatExperiments
      | ImpersonationChatExperiment
      | Posts
      | MoreTags
      | SendLinks
      | SendAudios
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
      bottom = ReceiveChats
      top = SendImages

instance BoundedEnum Privilege where
      cardinality = Cardinality 1

      fromEnum = case _ of
            ReceiveChats → 0
            StartChats → 100
            StartChatExperiments → 200
            ImpersonationChatExperiment → 201
            Posts → 202
            MoreTags → 300
            SendLinks → 400
            SendAudios → 401
            SendImages → 500

      toEnum = case _ of
            0 → Just ReceiveChats
            100 → Just StartChats
            200 → Just StartChatExperiments
            201 → Just ImpersonationChatExperiment
            202 -> Just Posts
            300 → Just MoreTags
            400 → Just SendLinks
            401 → Just SendAudios
            500 → Just SendImages
            _ → Nothing

instance Enum Privilege where
      succ = case _ of
            ReceiveChats → Just StartChats
            StartChats → Just StartChatExperiments
            StartChatExperiments → Just Posts
            ImpersonationChatExperiment → Nothing
            Posts -> Just MoreTags
            MoreTags → Just SendLinks
            SendLinks → Just SendImages
            SendAudios → Just SendImages
            SendImages → Nothing

      pred = case _ of
            ReceiveChats → Nothing
            StartChats → Just ReceiveChats
            StartChatExperiments → Just StartChats
            ImpersonationChatExperiment → Nothing
            Posts → Just StartChatExperiments
            MoreTags → Just Posts
            SendLinks → Just MoreTags
            SendAudios → Just SendLinks
            SendImages → Just SendAudios

instance Show Privilege where
      show = DSG.genericShow

instance ReadForeign Privilege where
      readImpl f = SU.fromJust <<< DE.toEnum <$> F.readInt f

instance WriteForeign Privilege where
      writeImpl = F.unsafeToForeign <<< DE.fromEnum

instance ToValue Privilege where
      toValue = F.unsafeToForeign <<< DE.fromEnum

instance FromValue Privilege where
      fromValue v = map (SU.fromJust <<< DE.toEnum) (DL.fromValue v ∷ Either String Int)
