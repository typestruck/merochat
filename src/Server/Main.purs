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
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Server.Configuration as CF
import Server.Database as SD
import Server.Routing as SR
import Server.WebSocket (Port(..))
import Server.WebSocket as SW
import Server.WebSocket.Events as SWE
import Shared.WebSocket.Options (port)

main :: Effect Unit
main = do
        configuration <- CF.readConfiguration
        startWebSocketServer configuration
        startHTTPServer configuration

startWebSocketServer :: Configuration -> Effect Unit
startWebSocketServer configuration = do
        connections <- ER.new DM.empty

        webSocketServer <- SW.createWebSocketServerWithPort (Port port) {} $ const (EC.log $ "WebSocket now up on ws://localhost:" <> show port)
        SW.onConnection webSocketServer $ SWE.handleConnection configuration connections
        SW.onServerError webSocketServer SWE.handleError

startHTTPServer :: Configuration -> Effect Unit
startHTTPServer c@(Configuration configuration) = do
        EA.launchAff_ do
                pool <- SD.newPool
                liftEffect $ H.serve configuration.port (router c pool) $ EC.log ("Server now up on http://localhost:" <> show configuration.port)
                where router configuration pool request@{ path } = do
                              session <- liftEffect $ SR.session configuration request
                              SR.runRouter { configuration, pool, session } request
