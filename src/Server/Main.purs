module Server.Main where

import Prelude

import Data.Foldable as DF
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Droplet.Driver.Internal.Query as DD
import Droplet.Driver.Migration as DDM
import Effect (Effect)
import Effect.Aff as EA
import Effect.Console as EC
import Effect.Ref as ER
import Effect.Timer as ET
import Payload.Server (defaultOpts)
import Payload.Server as PS
import Server.Configuration as CF
import Server.Database as SD
import Server.Guard (guards)
import Server.Handler as SH
import Server.Types (Configuration)
import Server.WebSocket (Port(..))
import Server.File as SF
import Server.WebSocket as SW
import Server.WebSocket.Events (aliveDelay)
import Environment(development)
import Server.WebSocket.Events as SWE
import Shared.Options.WebSocket (port)
import Shared.Spec (spec)

main ∷ Effect Unit
main = do
      configuration@{storageApplicationKeyId, storageApplicationKey} ← CF.readConfiguration
      when (not development) $ SF.init storageApplicationKeyId storageApplicationKey
      startWebSocketServer configuration
      startHTTPServer configuration

startWebSocketServer ∷ Configuration → Effect Unit
startWebSocketServer configuration = do
      allConnections ← ER.new DH.empty
      availability ← ER.new DH.empty
      webSocketServer ← SW.createWebSocketServerWithPort (Port port) {} $ const (EC.log $ "Web socket now up on ws://localhost:" <> show port)
      SW.onServerError webSocketServer SWE.handleError
      pool ← SD.newPool configuration
      SW.onConnection webSocketServer (SWE.handleConnection configuration pool allConnections availability)
      intervalID ← ET.setInterval aliveDelay (SWE.checkLastSeen allConnections availability)
      SW.onServerClose webSocketServer (const (ET.clearInterval intervalID))

startHTTPServer ∷ Configuration → Effect Unit
startHTTPServer configuration@{ port } = do
      pool ← SD.newPool configuration

      EA.launchAff_ $ PS.startGuarded (defaultOpts { port = port }) spec
            { guards: guards configuration
            , handlers: SH.handlers { configuration, pool, session: { userID: Nothing } }
            }
      EC.log $ "HTTP now up on http://localhost:" <> show port
