module Server.Main where

import Prelude

import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as EA
import Effect.Console as EC
import Effect.Ref (Ref)
import Effect.Ref as ER
import Effect.Timer as ET
import Payload.Server (defaultOpts)
import Payload.Server as PS
import Server.Configuration as CF
import Server.Database as SD
import Server.Guard (guards)
import Server.Handler as SH
import Server.Types (Configuration, StorageDetails)
import Server.WebSocket (Port(..))
import Server.WebSocket as SW
import Server.WebSocket.Events (aliveDelay)
import Server.WebSocket.Events as SWE
import Shared.Options.WebSocket (port)
import Shared.Spec (spec)

main :: Effect Unit
main = do
      configuration <- CF.readConfiguration
      storageRef <- createStorageDetails
      startWebSocketServer configuration storageRef
      startHTTPServer configuration storageRef

startWebSocketServer :: Configuration -> Ref StorageDetails -> Effect Unit
startWebSocketServer configuration storageDetails = do
      allConnections <- ER.new DH.empty
      webSocketServer <- SW.createWebSocketServerWithPort (Port port) {} $ const (EC.log $ "Web socket now up on ws://localhost:" <> show port)
      SW.onServerError webSocketServer SWE.handleError
      pool <- SD.newPool configuration
      SW.onConnection webSocketServer (SWE.handleConnection configuration pool allConnections storageDetails)
      intervalID <- ET.setInterval aliveDelay (SWE.checkLastSeen allConnections)
      SW.onServerClose webSocketServer (const (ET.clearInterval intervalID))

startHTTPServer :: Configuration -> Ref StorageDetails -> Effect Unit
startHTTPServer configuration@{port} storageDetails = do
      pool <- SD.newPool configuration
      EA.launchAff_ $ PS.startGuarded (defaultOpts { port = port }) spec {
            guards: guards configuration,
            handlers: SH.handlers { storageDetails, configuration, pool, session: { userID: Nothing } }
      }
      EC.log $ "HTTP now up on http://localhost:" <> show port

createStorageDetails :: Effect (Ref StorageDetails)
createStorageDetails = do
      authenticationKey <- CF.storageAuthenticationKey
      ER.new $ {
            accountAuthorizationToken: Nothing,
            uploadAuthorizationToken: Nothing,
            uploadUrl: Nothing,
            apiUrl: Nothing,
            authenticationKey
      }