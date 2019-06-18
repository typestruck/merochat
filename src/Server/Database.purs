module Server.Database where

import Database.PostgreSQL.Row
import Prelude

import Data.Decimal as S
import Data.Foldable as F
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.String as SS
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (Query(..), Pool(..))
import Database.PostgreSQL as P
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Run as R
import Run.Reader as RR

newPool ∷ Aff Pool
newPool = P.newPool $ (P.defaultPoolConfiguration "melanchat") { idleTimeoutMillis = Just 1000 }

-- singleBy :: forall a . FromSQLValue a => String -> By -> ServerEffect (Maybe a)
-- singleBy table by = do
-- 	{ pool } <- RR.ask
-- 	R.liftAff $ P.withConnection pool (\connection -> P.scalar connection (Query "select * from " <> table <> " where " <> column <> " = $1 ") Row1  )
-- 	where   column
-- 			| ID _ =  "id"
-- 			| Email _ = "email"
-- 			| Name _ = "name"


data Operator = Equals String String

data Logical = And Operator Logical | Or Operator Logical | Single Operator

data SQL = Select (Array String) | From String | Where (Array Logical)


g :: Array SQL -> String
g = F.foldl g' ""
	where   h s (And (Equals v v2) l) = s <> v <> " = " <> v2 <> h " and " l
		h s (Or (Equals v v2) l) = s <> v <> " = " <> v2 <> h " or " l
		h s (Single (Equals v v2)) = s <> v <> " = " <> v2

		g' s (Select fields) = s <> " select " <> SS.joinWith ", " fields
		g' s (From t) = s <> " from " <> t
		g' s (Where comparisions) = s <> " where " <> F.foldl h "" comparisions

class Selectable s where
	selectable ::  s -> Array SQL -> Array SQL
	selectable' :: s -> Array SQL

-- instance selectableUser :: Selectable User where
-- 	select u sql = select' u <> sql

-- 	select' u =
