module Main where

import Prelude (($), bind, otherwise, (&&), (==))
import Effect.Console as C
import HTTPure as H
import Response as R
import HTTPure(ServerM)
import HTTPure.Lookup((!@))
import Template.Landing as L
import Configuration as CF
import Effect.Class as E

--add here safe routing

main :: ServerM
main = do
        configuration <- CF.readConfiguration
        H.serve configuration.port (router configuration) $ C.log "Server now up on port 8000"
        where router configuration { path : [] } = do
                      html <- E.liftEffect L.landing
                      R.html html
              router configuration { path }
                        | configuration.development && path !@ 0 == "client" = R.serveDevelopmentFile (path !@ 1) (path !@ 2)
                        | otherwise = H.notFound