module Test.Server where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as EA
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

serverAction :: forall result. (Unit -> ServerEffect result) -> Effect Unit
serverAction action =
        EA.launchAff_ $ do
                pool <- SD.newPool
                R.runBaseAff' <<<
                RE.catch SRR.requestError <<<
                RS.evalState {
                        session : { user : Nothing }
                } <<<
                RR.runReader {
                        configuration,
                        pool
                } $ do
                        _ <- action unit
                        SRR.html ""

serverActionCatch :: forall result. (ResponseError -> Run (aff :: AFF, effect :: EFFECT) Unit) -> (Unit -> ServerEffect result) -> Effect Unit
serverActionCatch catch action  =
        EA.launchAff_ $ do
                pool <- SD.newPool
                R.runBaseAff' <<<
                RE.catch catch <<<
                RS.evalState {
                        session : { user : Nothing }
                } <<<
                RR.runReader {
                        configuration,
                        pool
                } $ do
                        _ <- action unit
                        SRR.html ""


