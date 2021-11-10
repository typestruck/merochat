-- | Types common to all server side modules
module Server.Types where

import Prelude

import Data.Enum (class BoundedEnum, Cardinality(..), class Enum)
import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic as DGRS
import Droplet.Driver (Pool, PgError)
import Effect.Ref (Ref)
import Payload.ContentType (html)
import Payload.Headers as PH
import Payload.ResponseTypes as PR
import Payload.Server.Response (class EncodeResponse)
import Run (AFF, Run, EFFECT)
import Run.Except (EXCEPT)
import Run.Reader (READER)
import Server.WebSocket (WebSocketConnection, AliveWebSocketConnection)
import Shared.ResponseError (ResponseError)
import Type.Row (type (+))

type Configuration =
      { port ∷ Int
      , captchaSecret ∷ String
      , tokenSecret ∷ String
      , salt ∷ String
      , databaseHost ∷ Maybe String
      , emailHost ∷ String
      , emailUser ∷ String
      , emailPassword ∷ String
      , randomizeProfiles ∷ Boolean
      }

type Session =
      { userID ∷ Maybe Int
      }

type BaseReader extension =
      { storageDetails ∷ Ref StorageDetails
      , pool ∷ Pool
      | extension
      }

type ServerReader = BaseReader
      ( configuration ∷ Configuration
      , session ∷ Session
      )

type BaseEffect r a = Run (READER r + EXCEPT ResponseError + AFF + EFFECT + ()) a

type DatabaseEffect a = Run (EXCEPT PgError + AFF + ()) a

type ServerEffect a = BaseEffect ServerReader a

type StorageDetails =
      { authenticationKey ∷ String
      , accountAuthorizationToken ∷ Maybe String
      , uploadAuthorizationToken ∷ Maybe String
      , uploadUrl ∷ Maybe String
      , apiUrl ∷ Maybe String
      }

data By
      = ID Int
      | Email String
