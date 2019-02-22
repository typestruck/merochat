module Main where

import Prelude (($), bind, otherwise)

import Effect.Console as C
import HTTPure as H
import Response as R
import HTTPure(ServerM)
import Template.Landing as L
import Configuration as CF
import Effect.Class as E

main :: ServerM
main = do
        config <- CF.readConfiguration
        H.serve config.port router $ C.log "Server now up on port 8000"
        where router { path : [] } = do
                      html <- E.liftEffect L.landing
                      R.html html
              router { path }
                        | config.development && path @! 0 == "client" = R.serveDevelopmentFile (path @! 1) (path @! 2)
                        | otherwise = H.notFound