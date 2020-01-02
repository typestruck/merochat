module Server.Database where

import Prelude
import Server.Types
import Shared.Types

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as DA
import Data.Array.Partial as DAA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Database.PostgreSQL (class FromSQLRow, class FromSQLValue, class ToSQLRow, Connection, Pool(..), Query(..), Row1(..))
import Database.PostgreSQL as DP
import Effect.Aff (Aff, Error)
import Effect.Aff as EA
import Partial.Unsafe as PU
import Run as R
import Run.Reader as RR

newPool ∷ Aff Pool
newPool = DP.newPool $ (DP.defaultPoolConfiguration "melanchat") {
        user = Just "melanchat",
        idleTimeoutMillis = Just 1000
}

insert :: forall r query parameters value. ToSQLRow value => Query query parameters -> value -> BaseEffect { pool :: Pool | r } PrimaryKey
insert query parameters = withConnection insertReturnID
        where   addReturnID (Query text) = Query $ text <> " returning id"

                insertReturnID connection = do
                        rows <- DP.scalar connection (addReturnID query) parameters
                        pure $ PU.unsafePartial (DM.fromJust rows)

scalar :: forall r query value. ToSQLRow query => FromSQLValue value => Query query (Row1 value) -> query -> BaseEffect { pool :: Pool | r } (Maybe value)
scalar query parameters = withConnection $ \connection -> DP.scalar connection query parameters

scalar' :: forall r query value. ToSQLRow query => FromSQLValue value => Query query (Row1 value) -> query -> BaseEffect { pool :: Pool | r } value
scalar' query parameters = withConnection $ \connection -> do
        rows <- DP.scalar connection query parameters
        pure $ PU.unsafePartial (DM.fromJust rows)

select :: forall r query row. ToSQLRow query => FromSQLRow row => Query query row -> query -> BaseEffect { pool :: Pool | r } (Array row)
select query parameters = withConnection $ \connection -> DP.query connection query parameters

single :: forall r query row. ToSQLRow query => FromSQLRow row => Query query row -> query -> BaseEffect { pool :: Pool | r }  (Maybe row)
single query parameters = withConnection $ \connection -> do
        rows <- DP.query connection query parameters
        toMaybe rows
        where toMaybe rows = do
                let length = DA.length rows
                if length == 0 then
                        pure Nothing -- as opposed to impure nothing
                 else if length == 1 then
                        pure <<< Just $ PU.unsafePartial (DAA.head rows)
                 else
                        EA.throwError $ EA.error "more than one row returned for single query"

single' :: forall r query row. ToSQLRow query ⇒ FromSQLRow row ⇒ Query query row → query → BaseEffect { pool :: Pool | r } row
single' query parameters = withConnection $ \connection -> do
        rows <- DP.query connection query parameters
        pure $ PU.unsafePartial (DAA.head rows)

execute :: forall r query parameters. ToSQLRow parameters => Query parameters query -> parameters -> BaseEffect { pool :: Pool | r } Unit
execute query parameters = withConnection $ \connection -> DP.execute connection query parameters

withConnection :: forall r result. (Connection -> Aff result) -> BaseEffect { pool :: Pool | r } result
withConnection runner = do
        { pool } <- RR.ask
        R.liftAff $ DP.withConnection pool runner