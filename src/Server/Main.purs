module Server.Main where

import Prelude (($), bind, otherwise, (&&), (==))
import Effect.Console as C
import HTTPure as H
import HTTPure(ServerM)
import Server.Configuration as CF
import Server.Routing as RO

--add here safe routing

main :: ServerM
main = do
        configuration <- CF.readConfiguration
        H.serve configuration.port (RO.router configuration) $ C.log "Server now up on port 8000"