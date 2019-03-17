module Server.Configuration where

import Prelude

import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Argonaut.Decode.Generic.Rep as DGR
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Encode.Generic.Rep as EGR
import Data.Argonaut.Parser as P
import Node.FS.Sync(readTextFile)
import Node.Encoding(Encoding(..))
import Effect.Aff(launchAff)
import Effect(Effect(..))
import Data.Either(either, fromRight)
import Effect.Exception(throwException, error)
import Server.Types
import Partial.Unsafe(unsafePartial)

readConfiguration :: Effect Configuration
readConfiguration = do
	contents <- readTextFile UTF8 "configuration.json"
	either (const $ throwException $ error "Could not parse configuration") (pure <<< unsafePartial (fromRight <<< DGR.genericDecodeJson)) $ P.jsonParser contents