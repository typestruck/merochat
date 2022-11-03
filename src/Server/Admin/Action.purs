module Server.Admin.Action where

import Prelude

import Debug (spy)
import Effect.Class (liftEffect)
import Environment (production)
import Run.Reader as RR
import Server.Response as SR
import Server.Token as ST
import Server.Types (ServerEffect)
import Server.WebSocket (WebSocketMessage(..))
import Server.WebSocket as SW
import Shared.Im.Types (WebSocketPayloadServer(..))
import Shared.Json as SJ
import Shared.Options.WebSocket (port)

ban ∷ Int → { id ∷ Int, secret ∷ String } → ServerEffect Unit
ban loggedUserId query = do
      { configuration: { adminSecret } } ← RR.ask
      if query.secret == adminSecret then do
            cookie ← ST.createToken loggedUserId
            liftEffect do
                  connection ← SW.createWebSocket ("ws://localhost:" <> show port) cookie
                  SW.onOpen connection $ \_ → SW.sendMessage connection <<< WebSocketMessage <<< SJ.toJson $ Ban query
      else
            SR.throwBadRequest "nope"