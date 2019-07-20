module Test.Server where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe (Maybe(..))
import Database.PostgreSQL (Pool)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import HTTPure (Response)
import Run (Run, AFF, EFFECT)
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Run.State as RS
import Server.Database as SD
import Server.Response as SRR

configuration :: Configuration
configuration = Configuration {
        port: 8000,
        development: true,
        captchaSecret: "",
        benderURL: "",
        useBender: false,
        tokenSecretGET: "abc",
        tokenSecretPOST: "def",
        salt: "ghi"
}

newTestPool ∷ Aff Pool
newTestPool = DP.newPool $ (DP.defaultPoolConfiguration "melanchatTest") {
        user = Just "melanchat",
        idleTimeoutMillis = Just 1000
}

--this needs to truncate tables after the test is run

serverAction :: (Unit -> ServerEffect Unit) -> Aff Unit
serverAction action = do
        pool <- newTestPool
        R.runBaseAff' <<<
        RE.catch (const (pure unit)) <<<
        RS.evalState {
                session : { user : Nothing }
        } <<<
        RR.runReader {
                configuration,
                pool
        } $ action unit

serverActionCatch :: (ResponseError -> Run (aff :: AFF, effect :: EFFECT) Unit) -> (Unit -> ServerEffect Unit) -> Aff Unit
serverActionCatch catch action  = do
        pool <- newTestPool
        R.runBaseAff' <<<
        RE.catch catch <<<
        RS.evalState {
                session : { user : Nothing }
        } <<<
        RR.runReader {
                configuration,
                pool
        } $ action unit
