module Server.Main where

import Server.Types

import Data.Maybe (Maybe(..))
import Effect.Console as C
import HTTPure (ServerM)
import HTTPure as H
import Prelude (($), bind, otherwise, (&&), (==))
import Run as R
import Run.Reader as RR
import Run.State as RS
import Server.Configuration as CF
import Server.Database as B
import Server.Routing as RO

--use https://pursuit.purescript.org/packages/purescript-run

main :: ServerM
main = do
        configuration <- CF.readConfiguration
        pool <- B.newPool
        H.serve configuration.port (R.runBaseAff <<< RS.runState {user : Nothing} <<< RR.runReader {configuration : configuration, pool : pool} <<< RO.router) $ C.log "Server now up on port 8000"