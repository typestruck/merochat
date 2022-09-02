module Server.Main where

import Prelude

import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as EA
import Effect.Console as EC
import Effect.Ref as ER
import Effect.Timer as ET
import Environment (production)
import Payload.Server (defaultOpts)
import Payload.Server as PS
import Server.Configuration as CF
import Server.Database as SD
import Server.File as SF
import Server.Guard (guards)
import Server.Handler as SH
import Server.Types (Configuration)
import Server.WebSocket (Port(..))
import Server.WebSocket as SW
import Server.WebSocket.Events (aliveDelay)
import Server.WebSocket.Events as SWE
import Shared.Options.WebSocket (port)
import Shared.Spec (spec)

main ∷ Effect Unit
main = do
      configuration@{storageApplicationKeyId, storageApplicationKey} ← CF.readConfiguration
      when production $ SF.init storageApplicationKeyId storageApplicationKey
      startWebSocketServer configuration
      startHTTPServer configuration

startWebSocketServer ∷ Configuration → Effect Unit
startWebSocketServer configuration = do
      userAvailability ← ER.new DH.empty
      webSocketServer ← SW.createWebSocketServerWithPort (Port port) {} $ const (EC.log $ "Web socket now up on ws://localhost:" <> show port)
      SW.onServerError webSocketServer SWE.handleError
      pool ← SD.newPool configuration
      SW.onConnection webSocketServer (SWE.handleConnection configuration pool userAvailability)
      intervalId ← ET.setInterval aliveDelay (SWE.checkLastSeen userAvailability *> SWE.persistLastSeen { pool, userAvailability})
      SW.onServerClose webSocketServer (const (ET.clearInterval intervalId))

startHTTPServer ∷ Configuration → Effect Unit
startHTTPServer configuration@{ port } = do
      pool ← SD.newPool configuration
      EA.launchAff_ $ void do
            PS.startGuarded (defaultOpts { port = port }) spec
                  { guards: guards configuration
                  , handlers: SH.handlers { configuration, pool, session: { userId: Nothing } }
                  }
      EC.log $ "HTTP now up on http://localhost:" <> show port
