module Server.Main where

import Prelude

import Control.Monad.Error.Class as CMEC
import Data.Map as DM
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Effect.Console as EC
import Effect.Exception as EE
import Effect.Ref as ER
import HTTPure as H
import Payload.Server (defaultOpts)
import Payload.Server as PS
import Server.Configuration as CF
import Server.Database as SD
import Server.Handler as SH
import Server.InternalError.Template as SIT
import Server.Response as SRR
import Server.Router as SR
import Shared.Spec (spec)
import Server.Types (Configuration(..), ContentType(..))
import Server.WebSocket (Port(..))
import Server.WebSocket as SW
import Server.WebSocket.Events as SWE
import Shared.WebSocketOptions (port)
import Server.Guard (guards)

main :: Effect Unit
main = do
      configuration <- CF.readConfiguration
      startWebSocketServer configuration
      startHTTPServer configuration

      startHTTPServer' configuration

startWebSocketServer :: Configuration -> Effect Unit
startWebSocketServer configuration = do
      allConnections <- ER.new DM.empty

      webSocketServer <- SW.createWebSocketServerWithPort (Port port) {} $ const (EC.log $ "WebSocket now up on ws://localhost:" <> show port)
      SW.onServerError webSocketServer SWE.handleError
      pool <- SD.newPool
      SW.onConnection webSocketServer (SWE.handleConnection configuration pool allConnections)

startHTTPServer :: Configuration -> Effect Unit
startHTTPServer configuration@{ port } = do
      pool <- SD.newPool
      void <<< H.serve port (router pool) <<< EC.log $ "Server now up on http://localhost:" <> show configuration.port
      where router pool request@{ path } = do
                  session <- liftEffect $ SR.session configuration request
                  SR.runRouter { configuration, pool, session } request `CMEC.catchError` internalError
            internalError error = do
                  contents <- liftEffect <<< SIT.template $ EE.message error
                  H.ok' (SRR.headerContentType $ show HTML) contents

startHTTPServer' :: Configuration -> Effect Unit
startHTTPServer' configuration = do
      pool <- SD.newPool
      EA.launchAff_ $ PS.startGuarded (defaultOpts { port = 8001 }) spec {
            guards: guards configuration,
            handlers: SH.handlers { configuration, pool, session: { userID: Nothing } }
      }
      EC.log $ "Payload now up on http://localhost:8001"