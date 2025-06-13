module Server.Main where

import Prelude

import Data.HashMap as DH
import Effect (Effect)
import Effect.Aff as EA
import Effect.Console as EC
import Effect.Ref as ER
import Effect.Timer as ET
import Payload.Server (defaultOpts)
import Payload.Server as PS
import Server.Configuration as CF
import Server.Database as SD
import Server.Effect (Configuration)
import Server.Guard (guards)
import Server.Handler as SH
import Server.WebSocket (Port(..))
import Server.WebSocket as SW
import Server.WebSocket.Events (inactiveInterval, availabilityInterval)
import Server.WebSocket.Events as SWE
import Shared.Options.WebSocket (localPort)
import Shared.Spec (spec)

main ∷ Effect Unit
main = do
      configuration ← CF.readConfiguration
      startWebSocketServer configuration
      startHttpServer configuration

startWebSocketServer ∷ Configuration → Effect Unit
startWebSocketServer configuration = do
      allUsersAvailabilityRef ← ER.new DH.empty
      webSocketServer ← SW.createWebSocketServerWithPort (Port localPort) {} $ const (EC.log $ "Web socket now up on ws://localhost:" <> show localPort)
      SW.onServerError webSocketServer SWE.handleError
      pool ← SD.newPool configuration
      SW.onConnection webSocketServer (SWE.handleConnection configuration pool allUsersAvailabilityRef)
      inactiveIntervalId ← ET.setInterval inactiveInterval (SWE.terminateInactive allUsersAvailabilityRef)
      availbitilityIntervallId ← ET.setInterval availabilityInterval (SWE.persistLastSeen pool allUsersAvailabilityRef )
      SW.onServerClose webSocketServer (const (ET.clearInterval inactiveIntervalId *> ET.clearInterval availbitilityIntervallId))

startHttpServer ∷ Configuration → Effect Unit
startHttpServer configuration@{ port } = do
      pool ← SD.newPool configuration
      EA.launchAff_ $ void do
            PS.startGuarded (defaultOpts { port = port }) spec
                  { guards: guards { configuration, pool }
                  , handlers: SH.handlers { configuration, pool }
                  }
      EC.log $ "HTTP now up on http://localhost:" <> show port
