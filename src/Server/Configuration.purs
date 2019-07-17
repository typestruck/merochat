module Server.Configuration where

import Prelude
import Server.Types

import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Parser as DAP
import Data.Either as DE
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class(liftEffect)
import Effect.Console as EC
import Effect.Exception as EE
import Node.Encoding (Encoding(..))
import Node.FS.Aff as NFA
import Partial.Unsafe (unsafePartial)

readConfiguration :: Aff Configuration
readConfiguration = do
	contents <- NFA.readTextFile UTF8 "configuration.json"
	DE.either (const $ EA.throwError $ EE.error "Could not parse configuration") (pure <<< unsafePartial (DE.fromRight <<< DADGR.genericDecodeJson)) $ DAP.jsonParser contents