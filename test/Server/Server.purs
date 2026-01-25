module Test.Server where

import Prelude
import Server.Effect
import Shared.Im.Types

import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Droplet.Driver (Pool)
import Droplet.Driver as DD
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class as EC
import Effect.Ref as ER
import Run (Run, AFF, EFFECT)
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Server.Database as SD
import Server.Environment (databaseHost)
import Shared.ResponseError (ResponseError(..))
import Test.Unit (failure) as TUA
import Test.Unit as TU
import Test.Unit.Assert (equal) as TUA
import Type.Row (type (+))

newTestPool ∷ Effect Pool
newTestPool =
      DD.newPool $ (DD.defaultConfiguration "merochat_test")
            { user = Just "merochat_test"
            , password = Just "merochat_test"
            , host = databaseHost
            , idleTimeoutMillis = Just 1000
            }

serverAction ∷ ∀ a. ServerEffect a → Aff Unit
serverAction action = do
      pool ← liftEffect  newTestPool
      allUserSubscriptionsRef <- EC.liftEffect $ ER.new DH.empty
      R.runBaseAff' <<< RE.catch (\ex → R.liftAff $ TUA.failure ("unexpected exception caught: " <> show ex)) <<< RR.runReader
            { pool
            , allUserSubscriptionsRef
            } $ do
            truncateTables
            void action

serverActionCatch ∷ ∀ a. (ResponseError → Run (AFF + EFFECT + ()) Unit) → ServerEffect a → Aff Unit
serverActionCatch c action = do
      pool ← liftEffect  newTestPool
      allUserSubscriptionsRef <- EC.liftEffect $ ER.new DH.empty
      R.runBaseAff' <<< RE.catch c <<< RR.runReader
            { pool
            , allUserSubscriptionsRef
            } $ do
            truncateTables
            void action

truncateTables ∷ ServerEffect Unit
truncateTables = SD.unsafeExecute "select truncate_tables()" {}

catch expected =
      case _ of
            BadRequest { reason } → R.liftAff $ TUA.equal expected reason
            other → R.liftAff <<< TU.failure $ "Unexpected exception: " <> show other
