module Server.Database where

import Prelude
import Server.Types
import Shared.Types

import Control.Monad.Except as CME
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Database.PostgreSQL (class FromSQLRow, class FromSQLValue, class ToSQLRow, Connection, PGError(..), Pool, Query(..), Row1)
import Database.PostgreSQL.PG as DP

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Effect.Exception.Unsafe as EEU
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Shared.Types as ST
import Shared.Unsafe as SU

--this makes pg interpret bigints as ints (since we don't use them) and dates as Number (to be used as epoch)
foreign import setUpConversions :: Effect Unit

newPool ∷ Configuration -> Effect Pool
newPool { databaseHost } = do
      setUpConversions
      DP.newPool $ (DP.defaultPoolConfiguration "melanchat") {
            user = Just "melanchat",
            host = databaseHost,
            idleTimeoutMillis = Just 1000
      }

insert :: forall r query parameters value. ToSQLRow value => Query query parameters -> value -> BaseEffect { pool :: Pool | r } PrimaryKey
insert query parameters = withConnection (insertReturnID query parameters)

insertWith :: forall value result query parameters. ToSQLRow value => FromSQLValue result => Connection -> Query query parameters -> value -> PG result
insertWith connection query parameters = insertReturnID query parameters connection

insertReturnID :: forall parameters query result value. ToSQLRow value => FromSQLValue result => Query query parameters -> value -> Connection -> PG result
insertReturnID query parameters connection = SU.fromJust <$>  DP.scalar connection (addReturnID query) parameters
      where addReturnID (Query text) = Query $ text <> " returning id"

scalar :: forall r query value. ToSQLRow query => FromSQLValue value => Query query (Row1 value) -> query -> BaseEffect { pool :: Pool | r } (Maybe value)
scalar query parameters = withConnection $ \connection -> DP.scalar connection query parameters

scalar' :: forall r query value. ToSQLRow query => FromSQLValue value => Query query (Row1 value) -> query -> BaseEffect { pool :: Pool | r } value
scalar' query parameters = withConnection $ \connection -> scalarWith connection query parameters

scalarWith :: forall value query. ToSQLRow query => FromSQLValue value => Connection -> Query query (Row1 value) -> query -> PG value
scalarWith connection query parameters = SU.fromJust <$> DP.scalar connection query parameters

select :: forall r query row. ToSQLRow query => FromSQLRow row => Query query row -> query -> BaseEffect { pool :: Pool | r } (Array row)
select query parameters = withConnection $ \connection -> DP.query connection query parameters

single :: forall r query row. ToSQLRow query => FromSQLRow row => Query query row -> query -> BaseEffect { pool :: Pool | r } (Maybe row)
single query parameters = withConnection $ \connection -> toMaybe <$> DP.query connection query parameters
      where toMaybe rows = do
                  let length = DA.length rows
                  if length == 0 then
                        Nothing -- as opposed to impure nothing
                  else if length == 1 then
                        DA.head rows
                  else
                        tooManyResults unit

tooManyResults :: forall error. Unit -> error
tooManyResults _ = EEU.unsafeThrow "single query resulted in no/more than one results"

single' :: forall r query row. ToSQLRow query ⇒ FromSQLRow row ⇒ Query query row → query → BaseEffect { pool :: Pool | r } row
single' query parameters = withConnection $ \connection -> singleWith connection query parameters

singleWith :: forall query row. ToSQLRow query => FromSQLRow row => Connection -> Query query row -> query -> PG row
singleWith connection query parameters = extract <$> DP.query connection query parameters
      where extract = case _ of
                  [r] -> r
                  _ -> tooManyResults unit

execute :: forall r query parameters. ToSQLRow parameters => Query parameters query -> parameters -> BaseEffect { pool :: Pool | r } Unit
execute query parameters = withConnection $ \connection -> executeWith connection query parameters

executeWith :: forall row query. ToSQLRow query => Connection -> Query query row -> query -> PG Unit
executeWith connection query parameters =  DP.execute connection query parameters

withConnection :: forall r result. (Connection -> PG result) -> BaseEffect { pool :: Pool | r } result
withConnection runner = do
      { pool } <- RR.ask
      eitherResult <- R.liftAff <<< CME.runExceptT $ DP.withConnection CME.runExceptT pool runner
      finish eitherResult
      where finish = case _ of
                  Right result ->
                        pure result
                  Left error ->
                        throwError error

withTransaction :: forall result r. (Connection -> PG result) -> BaseEffect { pool :: Pool | r } result
withTransaction runner = withConnection $ \connection -> DP.withTransaction CME.runExceptT connection (runner connection)

throwError :: forall r error. PGError -> BaseEffect { pool :: Pool | r } error
throwError error = do
      liftEffect $ EC.log errorMessage
      RE.throw $ ST.InternalError { reason: errorMessage, context: checkRevelanceError error }
      where errorMessage = show error
            checkRevelanceError = case _ of
                  IntegrityError _ -> Just MissingForeignKey
                  _ -> Nothing
