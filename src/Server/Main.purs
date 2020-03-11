module Server.Main where

import Prelude
import Server.Types

import Data.Map as DM
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Effect.Console as EC
import Effect.Ref as ER
import HTTPure as H
import Server.Configuration as CF
import Server.Database as SD
import Server.Routing as SR
import Server.WebSocket (Port(..))
import Server.WebSocket as SW
import Server.WebSocket.Events as SWE
import Shared.WebSocketOptions (port)

main :: Effect Unit
main = do
        configuration <- CF.readConfiguration
        startWebSocketServer configuration
        startHTTPServer configuration

startWebSocketServer :: Configuration -> Effect Unit
startWebSocketServer configuration = do
        allConnections <- ER.new DM.empty

        webSocketServer <- SW.createWebSocketServerWithPort (Port port) {} $ const (EC.log $ "WebSocket now up on ws://localhost:" <> show port)
        SW.onServerError webSocketServer SWE.handleError
        EA.launchAff_ do
                pool <- SD.newPool
                liftEffect <<< SW.onConnection webSocketServer $ SWE.handleConnection { configuration, pool, allConnections }

startHTTPServer :: Configuration -> Effect Unit
startHTTPServer c@(Configuration configuration) = do
        EA.launchAff_ do
                pool <- SD.newPool
                liftEffect $ H.serve configuration.port (router pool) $ EC.log ("Server now up on http://localhost:" <> show configuration.port)

        where   router pool request@{ path } = do
                        session <- liftEffect $ SR.session c request
                        SR.runRouter { configuration: c, pool, session } request
