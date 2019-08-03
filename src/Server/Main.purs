module Server.Main where

import Prelude
import Server.Types

import Effect (Effect)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Effect.Console as EC
import HTTPure as H
import Server.Configuration as CF
import Server.Database as SD
import Server.Routing as SR

main :: Effect Unit
main = EA.launchAff_ $ do
        c@(Configuration configuration) <- CF.readConfiguration
        pool <- SD.newPool
        liftEffect $ H.serve configuration.port (router c pool) $ EC.log "Server now up on http://localhost:8000"
        where router configuration pool request = do
                session <- liftEffect $ SR.session configuration request
                SR.runRouter {configuration, pool} { session } request
