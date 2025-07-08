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
import Server.Database as SD
import Server.Environment (port)
import Server.Guard as SG
import Server.Handler as SH
import Server.WebSocket (Port(..))
import Server.WebSocket as SW
import Server.WebSocket.Events (inactiveInterval)
import Server.WebSocket.Events as SWE
import Shared.Options.WebSocket (localPort)
import Shared.Spec (spec)

main ∷ Effect Unit
main = do
      startWebSocketServer
      startHttpServer

startWebSocketServer ∷ Effect Unit
startWebSocketServer = do
      allUsersAvailabilityRef ← ER.new DH.empty
      webSocketServer ← SW.createWebSocketServerWithPort (Port localPort) {} $ const (EC.log $ "Web socket now up on ws://localhost:" <> show localPort)
      SW.onServerError webSocketServer SWE.handleError
      pool ← SD.newPool
      SW.onConnection webSocketServer (SWE.handleConnection pool allUsersAvailabilityRef)
      inactiveIntervalId ← ET.setInterval inactiveInterval (SWE.terminateInactive pool allUsersAvailabilityRef)
      SW.onServerClose webSocketServer (const (ET.clearInterval inactiveIntervalId))

startHttpServer ∷ Effect Unit
startHttpServer = do
      pool ← SD.newPool
      EA.launchAff_ $ void do
            PS.startGuarded (defaultOpts { port = port }) spec
                  { guards: SG.guards { pool }
                  , handlers: SH.handlers { pool }
                  }