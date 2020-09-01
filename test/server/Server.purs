module Test.Server where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe (Maybe(..))
import Database.PostgreSQL (Pool, Query(..), Row0(..))
import Database.PostgreSQL as DP
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Process as NP
import Run (Run, AFF, EFFECT)
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Server.Configuration as SC
import Server.Database as SD
import Test.Unit as TU
import Test.Unit as TUA
import Test.Unit.Assert as TUA

getConfiguration :: Effect Configuration
getConfiguration = do
      NP.setEnv "DEVELOPMENT" "true"
      SC.readConfiguration

session :: Session
session = { userID : Nothing }

newTestPool ∷ Effect Pool
newTestPool = DP.newPool $ (DP.defaultPoolConfiguration "melanchatTest") {
      user = Just "melanchat",
      idleTimeoutMillis = Just 1000
}

serverAction :: forall a. ServerEffect a -> Aff Unit
serverAction action = do
      pool <- liftEffect $ newTestPool
      configuration <- liftEffect getConfiguration
      R.runBaseAff' <<< RE.catch (\ex -> R.liftAff $ TUA.failure ("unexpected exception caught: " <> show ex)) <<< RR.runReader {
            configuration,
            pool,
            session
      } $ do
            truncateTables
            void action

serverActionCatch :: forall a. (ResponseError -> Run (aff :: AFF, effect :: EFFECT) Unit) -> ServerEffect a -> Aff Unit
serverActionCatch catch action  = do
      pool <- liftEffect $ newTestPool
      configuration <- liftEffect getConfiguration
      R.runBaseAff' <<< RE.catch catch <<< RR.runReader {
            configuration,
            pool,
            session
      } $ do
            truncateTables
            void action

truncateTables :: ServerEffect Unit
truncateTables = SD.execute (Query "select truncateTables()") Row0

catch expected =
      case _ of
            BadRequest { reason } -> R.liftAff $ TUA.equal expected reason
            other -> R.liftAff <<< TU.failure $ "Unexpected exception: " <> show other
