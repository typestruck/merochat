module Server.Database where

import Prelude
import Server.Types
import Shared.Types

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Error.Class as CMEC
import Data.Array as DA
import Data.Array.Partial as DAA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Database.PostgreSQL (class FromSQLRow, class FromSQLValue, class ToSQLRow, Connection, Pool(..), Query(..), Row1(..))
import Database.PostgreSQL as DP
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Effect.Console as EC
import Effect.Exception as EE
import Partial.Unsafe as PU
import Run as R
import Run.Reader as RR
import Shared.Unsafe as SU

newPool ∷ Effect Pool
newPool = DP.newPool $ (DP.defaultPoolConfiguration "melanchat") {
      user = Just "melanchat",
      idleTimeoutMillis = Just 1000
}

insert :: forall r query parameters value. ToSQLRow value => Query query parameters -> value -> BaseEffect { pool :: Pool | r } PrimaryKey
insert query parameters = withConnection (insertReturnID query parameters)

insertWith connection query parameters = insertReturnID query parameters connection

insertReturnID query parameters connection = do
      rows <- DP.scalar connection (addReturnID query) parameters
      rows' <- runLogEither "insertReturnID" rows
      pure $ SU.fromJust "insertReturnID" rows'
      where addReturnID (Query text) = Query $ text <> " returning id"

scalar :: forall r query value. ToSQLRow query => FromSQLValue value => Query query (Row1 value) -> query -> BaseEffect { pool :: Pool | r } (Maybe value)
scalar query parameters = withConnection $ \connection -> do
      rows <- DP.scalar connection query parameters
      runLogEither "scalar" rows

scalar' :: forall r query value. ToSQLRow query => FromSQLValue value => Query query (Row1 value) -> query -> BaseEffect { pool :: Pool | r } value
scalar' query parameters = withConnection $ \connection -> scalarWith connection query parameters

scalarWith connection query parameters = do
      rows <- DP.scalar connection query parameters
      rows' <- runLogEither "scalarWith" rows
      pure $ SU.fromJust "scalarWith" rows'

select :: forall r query row. ToSQLRow query => FromSQLRow row => Query query row -> query -> BaseEffect { pool :: Pool | r } (Array row)
select query parameters = withConnection $ \connection -> do
      rows <- DP.query connection query parameters
      runLogEither "select" rows

single :: forall r query row. ToSQLRow query => FromSQLRow row => Query query row -> query -> BaseEffect { pool :: Pool | r }  (Maybe row)
single query parameters = withConnection $ \connection -> do
      rows <- DP.query connection query parameters
      rows' <- runLogEither "single" rows
      toMaybe rows'
      where toMaybe rows = do
                  let length = DA.length rows
                  if length == 0 then
                        pure Nothing -- as opposed to impure nothing
                  else if length == 1 then
                        pure <<< Just $ PU.unsafePartial (DAA.head rows)
                  else
                        EA.throwError $ EA.error "more than one row returned for single query"

single' :: forall r query row. ToSQLRow query ⇒ FromSQLRow row ⇒ Query query row → query → BaseEffect { pool :: Pool | r } row
single' query parameters = withConnection $ \connection -> singleWith connection query parameters

singleWith connection query parameters = do
      rows <- DP.query connection query parameters
      rows' <- runLogEither "singleWith" rows
      pure $ PU.unsafePartial (DAA.head rows')

execute :: forall r query parameters. ToSQLRow parameters => Query parameters query -> parameters -> BaseEffect { pool :: Pool | r } Unit
execute query parameters = withConnection $ \connection -> executeWith connection query parameters

executeWith connection query parameters = do
      result <- DP.execute connection query parameters
      runLogMaybe result

withConnection :: forall r result. (Connection -> Aff result) -> BaseEffect { pool :: Pool | r } result
withConnection runner = do
      { pool } <- RR.ask
      R.liftAff $ DP.withConnection pool runLogPGError
      where runLogPGError =
                  case _ of
                        Right connection -> runner connection
                        Left error -> throwError "internal connection error" error

--REFACTOR: clean this shit up
runLogMaybe =
      case _ of
           Nothing -> pure unit
           Just error -> throwError "internal maybe error" error

runLogEither from =
      case _ of
           Right r -> pure r
           Left error -> throwError ("internal either error " <> from) error

throwError log error = do
      liftEffect <<< EC.log  $ log <> show error
      CMEC.throwError <<< EE.error $ show error

withTransaction runner = withConnection $ \connection -> do
      result <- DP.withTransaction connection (runner connection)
      runLogEither "transaction" result