module Shared.Json where

import Prelude

import Data.Argonaut.Core as DAC
import Data.Argonaut.Decode.Generic (class DecodeRep)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic (class EncodeRep)
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Argonaut.Parser as DAP
import Data.Either (Either(..))
import Data.Either as DT
import Data.Generic.Rep (class Generic)

fromJSON ∷ ∀ v value. Generic value v ⇒ DecodeRep v ⇒ String → Either String value
fromJSON content = do
      json ← DAP.jsonParser content
      DT.either (Left <<< show) Right $ DADGR.genericDecodeJson json

toJson ∷ ∀ v value. Generic value v ⇒ EncodeRep v ⇒ value → String
toJson = DAC.stringify <<< DAEGR.genericEncodeJson
