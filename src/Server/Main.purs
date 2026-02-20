module Server.Main where

import Prelude

import Data.HashMap (HashMap)
import Data.HashMap as DH
import Droplet.Driver (Pool)
import Effect (Effect)
import Effect.Aff as EA
import Effect.Console as EC
import Effect.Ref (Ref)
import Effect.Ref as ER
import Effect.Timer as ET
import Payload.Server (defaultOpts)
import Payload.Server as PS
import Server.Database as SD
import Server.Environment (port)
import Server.Guard as SG
import Server.Handler as SH
import Server.Push as SP
import Server.WebSocket (Port(..))
import Server.WebSocket as SW
import Server.WebSocket.Events (inactiveInterval)
import Server.WebSocket.Events as SWE
import Shared.Options.WebSocket (localPort)
import Shared.Spec (spec)

main ∷ Effect Unit
main = do
      pool ← SD.newPool
      allUserSubscriptionsRef ← ER.new DH.empty
      SP.initiazeSubscriptions pool allUserSubscriptionsRef
      startWebSocketServer pool allUserSubscriptionsRef
      startHttpServer pool allUserSubscriptionsRef

startWebSocketServer ∷ Pool → Ref (HashMap Int (Array String)) → Effect Unit
startWebSocketServer pool allUserSubscriptionsRef = do
      allUsersAvailabilityRef ← ER.new DH.empty
      webSocketServer ← SW.createWebSocketServerWithPort (Port localPort) {} $ const (EC.log $ "Web socket now up on ws://localhost:" <> show localPort)
      SW.onServerError webSocketServer SWE.handleError
      SW.onConnection webSocketServer (SWE.handleConnection pool allUsersAvailabilityRef allUserSubscriptionsRef)
      inactiveIntervalId ← ET.setInterval inactiveInterval (SWE.terminateInactive pool allUsersAvailabilityRef)
      SW.onServerClose webSocketServer (const (ET.clearInterval inactiveIntervalId))

startHttpServer ∷ Pool → Ref (HashMap Int (Array String)) → Effect Unit
startHttpServer pool allUserSubscriptionsRef =
      EA.launchAff_ $ void do
            PS.startGuarded (defaultOpts { port = port }) spec
                  { guards: SG.guards { pool }
                  , handlers: SH.handlers { pool, allUserSubscriptionsRef }
                  }