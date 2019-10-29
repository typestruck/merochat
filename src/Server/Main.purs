module Server.Main where

import Prelude
import Server.Types

import Effect (Effect)
import Shared.WebSocket.Options(port)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Effect.Console as EC
import HTTPure as H
import Server.Configuration as CF
import Server.Database as SD
import Server.Routing as SR
import Server.WS (Port(..))
import Server.WS as SW

main :: Effect Unit
main = EA.launchAff_ $ do
        c@(Configuration configuration) <- CF.readConfiguration
        pool <- SD.newPool
        webSocketServer <- SW.createWebSocketServerWithPort (Port port) {} $ const (EC.log $ "WebSocket now up on ws://localhost:" <> show port)
        liftEffect $ H.serve configuration.port (router c pool) $ EC.log ("Server now up on http://localhost:" <> show configuration.port)
        where router configuration pool request@{ path } = do
                session <- liftEffect $ SR.session configuration request
                SR.runRouter { configuration, pool, session, webSocketServer } request
