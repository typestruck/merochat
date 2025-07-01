module Shared.InternalHelp.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Generic.Rep (class Generic)
import Shared.Modal.Types (ScreenModal)

type InternalHelpModel =
      { toggleHelp ∷ DisplayHelpSection,
      visible :: Boolean
      }

data InternalHelpMessage =
      ToggleVisibility ScreenModal
      | ToggleHelpSection DisplayHelpSection

data DisplayHelpSection
      = FAQ
      | Terms
      | Privacy

instance EncodeJson DisplayHelpSection where
      encodeJson = DAEGR.genericEncodeJson

derive instance Eq DisplayHelpSection

instance DecodeJson DisplayHelpSection where
      decodeJson = DADGR.genericDecodeJson

derive instance Generic DisplayHelpSection _