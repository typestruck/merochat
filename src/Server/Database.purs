module Server.Database where

import Prelude
import Server.Types

import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Database.PostgreSQL (class FromSQLValue, class ToSQLRow, Connection, Pool(..), Query(..), Row1(..))
import Database.PostgreSQL as DP
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Partial.Unsafe as PU
import Run as R
import Run.Reader as RR

newPool ∷ Aff Pool
newPool = DP.newPool $ (DP.defaultPoolConfiguration "melanchat") {
        user = Just "melanchat",
        idleTimeoutMillis = Just 1000
}

insert query row = withConnection insertReturnID
        where   addReturnID (Query text) = Query $ text <> " returning id"

                insertReturnID connection = do
                        result <- DP.scalar connection (addReturnID query) row
                        pure $ PU.unsafePartial (DM.fromJust result)

scalar query row = withConnection (\connection -> DP.scalar connection query row)

withConnection runner = do
        { pool } <- RR.ask
        R.liftAff $ DP.withConnection pool runner

-- singleBy :: forall a . FromSQLValue a => String -> By -> ServerEffect (Maybe a)
-- singleBy table by = do
-- 	{ pool } <- RR.ask
-- 	R.liftAff $ P.withConnection pool (\connection -> P.scalar connection (Query "select * from " <> table <> " where " <> column <> " = $1 ") Row1  )
-- 	where   column
-- 			| ID _ =  "id"
-- 			| Email _ = "email"
-- 			| Name _ = "name"


-- data Operator = Equals String String

-- data Logical = And Operator Logical | Or Operator Logical | Single Operator

-- data SQL = Select (Array String) | From String | Where (Array Logical)


-- g :: Array SQL -> String
-- g = F.foldl g' ""
-- 	where   h s (And (Equals v v2) l) = s <> v <> " = " <> v2 <> h " and " l
-- 		h s (Or (Equals v v2) l) = s <> v <> " = " <> v2 <> h " or " l
-- 		h s (Single (Equals v v2)) = s <> v <> " = " <> v2

-- 		g' s (Select fields) = s <> " select " <> SS.joinWith ", " fields
-- 		g' s (From t) = s <> " from " <> t
-- 		g' s (Where comparisions) = s <> " where " <> F.foldl h "" comparisions

-- class Selectable s where
-- 	selectable ::  s -> Array SQL -> Array SQL
-- 	selectable' :: s -> Array SQL

-- -- instance selectableUser :: Selectable User where
-- -- 	select u sql = select' u <> sql

-- -- 	select' u =
