module Server.Main where

import Control.Monad.Error.Class as CMEC
import Data.Map as DM
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Effect.Exception as EE
import Effect.Ref as ER
import HTTPure as H
import Prelude
import Server.Configuration as CF
import Server.Database as SD
import Server.InternalError.Template as SIT
import Server.Response as SRR
import Server.Router as SR
import Server.Types (Configuration(..), ContentType(..))
import Server.WebSocket (Port(..))
import Server.WebSocket as SW
import Server.WebSocket.Events as SWE
import Shared.WebSocketOptions (port)

--consider using a servant like framework?
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
      pool <- SD.newPool
      SW.onConnection webSocketServer (SWE.handleConnection configuration pool allConnections)

startHTTPServer :: Configuration -> Effect Unit
startHTTPServer c@(Configuration configuration) = do
      pool <- SD.newPool
      void <<< H.serve configuration.port (router pool) <<< EC.log $ "Server now up on http://localhost:" <> show configuration.port
      where router pool request@{ path } = do
                  session <- liftEffect $ SR.session c request
                  SR.runRouter { configuration: c, pool, session } request `CMEC.catchError` internalError
            internalError error = do
                  contents <- liftEffect <<< SIT.template $ EE.message error
                  H.ok' (SRR.headerContentType $ show HTML) contents


