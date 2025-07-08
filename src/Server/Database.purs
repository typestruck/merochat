module Server.Database (query, single, singleWith, execute, executeWith, unsafeQuery, unsafeQueryWith, unsafeSingle, unsafeSingleWith, unsafeExecute, unsafeExecuteWith, newPool, withTransaction) where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as CME
import Data.Either (Either(..))
import Data.Either as DT
import Data.Maybe (Maybe(..))
import Droplet.Driver (class FromResult, Connection, PgError(..), Pool)
import Droplet.Driver as DD
import Droplet.Driver.Unsafe as DDU
import Droplet.Language (class ToParameters)
import Droplet.Language.Internal.Translate (class ToQuery)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Prim.RowList (class RowToList)
import Run (Run, AFF)
import Run as R
import Run.Except (EXCEPT)
import Run.Except as RE
import Run.Reader as RR
import Server.Effect (BaseEffect)
import Server.Environment (databaseHost)
import Shared.ResponseError (DatabaseError(..))
import Shared.ResponseError as ST
import Type.Row (type (+))

type DatabaseEffect a = Run (EXCEPT PgError + AFF + ()) a

query ∷
      ∀ sql result r projection.
      ToQuery sql result ⇒
      RowToList result projection ⇒
      FromResult projection (Record result) ⇒
      sql →
      BaseEffect { pool ∷ Pool | r } (Array (Record result))
query q = withConnection (flip DD.query q)

single ∷
      ∀ r sql result projection.
      ToQuery sql result ⇒
      RowToList result projection ⇒
      FromResult projection (Record result) ⇒
      sql →
      BaseEffect { pool ∷ Pool | r } (Maybe (Record result))
single q = withConnection (flip DD.single q)

singleWith ∷
      ∀ sql result projection.
      ToQuery sql result ⇒
      RowToList result projection ⇒
      FromResult projection (Record result) ⇒
      Connection →
      sql →
      DatabaseEffect (Maybe (Record result))
singleWith connection q = hoistTransaction $ DD.single connection q

execute ∷ ∀ r sql. ToQuery sql () ⇒ sql → BaseEffect { pool ∷ Pool | r } Unit
execute q = withConnection $ \connection → hoist <$> DD.execute connection q

executeWith ∷ ∀ sql. ToQuery sql (()) ⇒ Connection → sql → DatabaseEffect Unit
executeWith connection q = hoistTransaction (hoist <$> DD.execute connection q)

unsafeQuery ∷
      ∀ r result projection parameters pra.
      RowToList result projection ⇒
      RowToList parameters pra ⇒
      ToParameters parameters pra ⇒
      FromResult projection (Record result) ⇒
      String →
      Record parameters →
      BaseEffect { pool ∷ Pool | r } (Array (Record result))
unsafeQuery q parameters = withConnection $ \connection → DDU.unsafeQuery connection Nothing q parameters

unsafeQueryWith ∷
      ∀ result projection parameters pra.
      RowToList result projection ⇒
      RowToList parameters pra ⇒
      ToParameters parameters pra ⇒
      FromResult projection (Record result) ⇒
      Connection →
      String →
      Record parameters →
      DatabaseEffect (Array (Record result))
unsafeQueryWith connection q parameters = hoistTransaction $ DDU.unsafeQuery connection Nothing q parameters

unsafeSingle ∷
      ∀ r result projection parameters pra.
      RowToList result projection ⇒
      RowToList parameters pra ⇒
      ToParameters parameters pra ⇒
      FromResult projection (Record result) ⇒
      String →
      Record parameters →
      BaseEffect { pool ∷ Pool | r } (Maybe (Record result))
unsafeSingle q parameters = withConnection $ \connection → DDU.unsafeSingle connection Nothing q parameters

unsafeSingleWith ∷
      ∀ result projection parameters pra.
      RowToList result projection ⇒
      RowToList parameters pra ⇒
      ToParameters parameters pra ⇒
      Connection →
      FromResult projection (Record result) ⇒
      String →
      Record parameters →
      DatabaseEffect (Maybe (Record result))
unsafeSingleWith connection q parameters = hoistTransaction $ DDU.unsafeSingle connection Nothing q parameters

unsafeExecute ∷
      ∀ r parameters pra.
      RowToList parameters pra ⇒
      ToParameters parameters pra ⇒
      String →
      Record parameters →
      BaseEffect { pool ∷ Pool | r } Unit
unsafeExecute q parameters = withConnection $ \connection → hoist <$> DDU.unsafeExecute connection Nothing q parameters

unsafeExecuteWith ∷
      ∀ parameters pra.
      RowToList parameters pra ⇒
      ToParameters parameters pra ⇒
      Connection →
      String →
      Record parameters →
      DatabaseEffect Unit
unsafeExecuteWith connection q parameters = hoistTransaction (hoist <$> DDU.unsafeExecute connection Nothing q parameters)

hoist ∷ Maybe PgError → Either PgError Unit
hoist = case _ of
      Nothing → Right unit
      Just err → Left err

hoistTransaction ∷ ∀ result. Aff (Either PgError result) → DatabaseEffect result
hoistTransaction action = do
      result ← R.liftAff action
      case result of
            Right r → pure r
            Left l → RE.throw l

newPool ∷  Effect Pool
newPool =
      DD.newPool $ (DD.defaultConfiguration "merochat")
            { user = Just "merochat"
            , host = databaseHost
            , idleTimeoutMillis = Just 1000
            }

withConnection ∷ ∀ r result. (Connection → Aff (Either PgError result)) → BaseEffect { pool ∷ Pool | r } result
withConnection handler = do
      { pool } ← RR.ask
      result ← R.liftAff $ DD.withConnection pool runConnection
      finish result
      where
      runConnection = case _ of
            Right c → handler c
            Left err → pure $ Left err

      finish = case _ of
            Right result → pure result
            Left error → throwError error

withTransaction ∷ ∀ result r. (Connection → DatabaseEffect result) → BaseEffect { pool ∷ Pool | r } result
withTransaction handler = do
      { pool } ← RR.ask
      result ← R.liftAff $ DD.withTransaction pool (R.runBaseAff <<< RE.runExcept <<< handler)
      finish result
      where
      finish = case _ of
            Right result → DT.either throwError pure result
            Left error → throwError error

throwError ∷ ∀ r error. PgError → BaseEffect { pool ∷ Pool | r } error
throwError error = do
      liftEffect $ EC.log errorMessage
      RE.throw $ ST.InternalError { reason: errorMessage, context: checkRelevanceError error }
      where
      errorMessage = show error
      checkRelevanceError = case _ of
            --this is absolutely vile
            IntegrityError _ → Just MissingForeignKey
            _ → Nothing
