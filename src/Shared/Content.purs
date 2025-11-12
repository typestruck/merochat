module Shared.Content where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic as DGRS
import Foreign as F
import Shared.Json as SJ
import Shared.Unsafe as SU

data Content
      = Image String Int Int String
      | Text String
      | Link String (Maybe String)
      | Audio String

instance DecodeJson Content where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson Content where
      encodeJson = DAEGR.genericEncodeJson

instance Show Content where
      show = DGRS.genericShow

derive instance Generic Content _