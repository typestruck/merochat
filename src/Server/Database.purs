module Server.Database where

import Prelude
import Server.Types

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
import Shared.Types
import Run.Reader as RR

newPool ∷ Aff Pool
newPool = DP.newPool $ (DP.defaultPoolConfiguration "melanchat") {
        user = Just "melanchat",
        idleTimeoutMillis = Just 1000
}

insert :: forall query parameters value. ToSQLRow value => Query query parameters -> value -> ServerEffect PrimaryKey
insert query parameters = withConnection insertReturnID
        where   addReturnID (Query text) = Query $ text <> " returning id"

                insertReturnID connection = do
                        rows <- DP.scalar connection (addReturnID query) parameters
                        pure $ PU.unsafePartial (DM.fromJust rows)

scalar :: forall query value. ToSQLRow query => FromSQLValue value => Query query (Row1 value) -> query -> ServerEffect (Maybe value)
scalar query parameters = withConnection $ \connection -> DP.scalar connection query parameters

scalar' :: forall query value. ToSQLRow query => FromSQLValue value => Query query (Row1 value) -> query -> ServerEffect value
scalar' query parameters = withConnection $ \connection -> do
        rows <- DP.scalar connection query parameters
        pure $ PU.unsafePartial (DM.fromJust rows)

single :: forall query row. ToSQLRow query => FromSQLRow row => Query query row -> query -> ServerEffect (Maybe row)
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

single' :: forall query row. ToSQLRow query ⇒ FromSQLRow row ⇒ Query query row → query → ServerEffect row
single' query parameters = withConnection $ \connection -> do
        rows <- DP.query connection query parameters
        pure $ PU.unsafePartial (DAA.head rows)

execute :: forall query parameters. ToSQLRow parameters => Query parameters query -> parameters -> ServerEffect Unit
execute query parameters = withConnection $ \connection -> DP.execute connection query parameters

withConnection :: forall result. (Connection -> Aff result) -> ServerEffect result
withConnection runner = do
        { pool } <- RR.ask
        R.liftAff $ DP.withConnection pool runner

-- singleBy :: forall a . FromSQLValue a => String -> By -> ServerEffect (Maybe a)
-- singleBy table by = do
--         { pool } <- RR.ask
--         R.liftAff $ P.withConnection pool (\connection -> P.scalar connection (Query "select * from " <> table <> " where " <> column <> " = $1 ") Row1  )
--         where   column
--                         | ID _ =  "id"
--                         | Email _ = "email"
--                         | Name _ = "name"


-- data Operator = Equals String String

-- data Logical = And Operator Logical | Or Operator Logical | Single Operator

-- data SQL = Select (Array String) | From String | Where (Array Logical)


-- g :: Array SQL -> String
-- g = F.foldl g' ""
--         where   h s (And (Equals v v2) l) = s <> v <> " = " <> v2 <> h " and " l
--                 h s (Or (Equals v v2) l) = s <> v <> " = " <> v2 <> h " or " l
--                 h s (Single (Equals v v2)) = s <> v <> " = " <> v2

--                 g' s (Select fields) = s <> " select " <> SS.joinWith ", " fields
--                 g' s (From t) = s <> " from " <> t
--                 g' s (Where comparisions) = s <> " where " <> F.foldl h "" comparisions

-- class Selectable s where
--         selectable ::  s -> Array SQL -> Array SQL
--         selectable' :: s -> Array SQL

-- -- instance selectableUser :: Selectable User where
-- --         select u sql = select' u <> sql

-- --         select' u =
