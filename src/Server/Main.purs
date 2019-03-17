module Server.Main where

import Prelude
import Server.Types

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class as EC
import Effect.Aff as A
import Effect.Console as C
import HTTPure (ServerM)
import HTTPure as H
import Run as R
import Run.Reader as RR
import Run.State as RS
import Server.Configuration as CF
import Server.Database as D
import Server.Routing as RO

main :: Effect Unit
main = A.launchAff_ $ do
        c@(Configuration configuration) <- CF.readConfiguration
        pool <- D.newPool
        EC.liftEffect $ H.serve configuration.port (run c pool <<< RO.router) $ C.log "Server now up on port 8000"
                --needs error strategy as well as logging
        where   run configuration pool =
                        R.runBaseAff' <<<
                        RS.evalState {
                                session : { user : Nothing }
                        } <<<
                        RR.runReader {
                                configuration : configuration,
                                pool : pool
                        }