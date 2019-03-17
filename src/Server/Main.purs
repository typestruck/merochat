module Server.Main where

import Prelude (($), bind, otherwise, (&&), (==))
import Effect.Console as C
import HTTPure as H
import HTTPure(ServerM)
import Server.Configuration as CF
import Server.Routing as RO

--configuration, database pool, session have to be threaded around
-- thus needing a state monad of some sort
-- check if algebraic effects or monads transformers are more convenient in purescript

main :: ServerM
main = do
        configuration <- CF.readConfiguration
        H.serve configuration.port (RO.router configuration) $ C.log "Server now up on port 8000"