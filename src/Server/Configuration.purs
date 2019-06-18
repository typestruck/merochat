module Server.Configuration where

import Prelude
import Server.Types

import Data.Argonaut.Decode.Generic.Rep as DGR
import Data.Argonaut.Parser as P
import Data.Either (either, fromRight)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)

readConfiguration :: Aff Configuration
readConfiguration = do
	contents <- readTextFile UTF8 "configuration.json"
	either (const $ throwError $ error "Could not parse configuration") (pure <<< unsafePartial (fromRight <<< DGR.genericDecodeJson)) $ P.jsonParser contents