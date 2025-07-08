module Server.Admin.Action where

import Prelude

import Debug (spy)
import Effect.Class (liftEffect)
import Run.Reader as RR
import Server.Effect (ServerEffect)
import Server.Environment (adminSecret)
import Server.Response as SR
import Server.Token as ST
import Server.WebSocket (WebSocketMessage(..))
import Server.WebSocket as SW
import Shared.Im.Types (WebSocketPayloadServer(..))
import Shared.Json as SJ
import Shared.Options.WebSocket (localPort)

ban ∷ Int → { id ∷ Int, secret ∷ String } → ServerEffect Unit
ban loggedUserId query = do
      if query.secret == adminSecret then do
            cookie ← ST.createToken loggedUserId
            liftEffect do
                  connection ← SW.createWebSocket ("ws://localhost:" <> show localPort) cookie
                  SW.onOpen connection $ \_ → SW.sendMessage connection <<< WebSocketMessage <<< SJ.toJson $ Ban query
      else
            SR.throwBadRequest "nope"