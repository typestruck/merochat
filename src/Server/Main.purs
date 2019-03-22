module Server.Main where

import Prelude
import Server.Types

import Data.Either as DET
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as A
import Effect.Class as EC
import Effect.Console as C
import HTTPure (ServerM)
import HTTPure as H
import Server.Configuration as CF
import Server.Database as D
import Server.Routing as RO
import Shared.Types ()

main :: Effect Unit
main = A.launchAff_ $ do
        c@(Configuration configuration) <- CF.readConfiguration
        pool <- D.newPool
        EC.liftEffect $ H.serve configuration.port (RO.runRouter {c, pool}) $ C.log "Server now up on port 8000"

