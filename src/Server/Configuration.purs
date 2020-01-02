module Server.Configuration where

import Prelude
import Server.Types

import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Parser as DAP
import Data.Either as DE
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Effect.Exception as EE
import Node.Encoding (Encoding(..))
import Node.FS.Sync as NFS
import Partial.Unsafe (unsafePartial)

readConfiguration :: Effect Configuration
readConfiguration = do
        contents <- NFS.readTextFile UTF8 "configuration.json"
        DE.either (const $ EE.throw "Could not parse configuration") (pure <<< unsafePartial (DE.fromRight <<< DADGR.genericDecodeJson)) $ DAP.jsonParser contents