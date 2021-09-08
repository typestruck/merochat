-- | Types common to all server side modules
module Server.Types where

import Prelude
import Shared.Types (ResponseError)

import Data.Enum (class BoundedEnum, Cardinality(..), class Enum)
import Data.HashMap (HashMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Type.Row (type (+))
import Effect.Ref (Ref)
import Droplet.Driver (Pool, PgError)
import Payload.ContentType (html)
import Payload.Headers as PH
import Payload.ResponseTypes as PR
import Payload.Server.Response (class EncodeResponse)
import Run (AFF, Run, EFFECT)
import Run.Except (EXCEPT)
import Run.Reader (READER)
import Server.WebSocket (WebSocketConnection, AliveWebSocketConnection)

type Ok = Record ()

newtype Html = Html String

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

type WebSocketReader = BaseReader
      ( sessionUserID ∷ Int
      , connection ∷ WebSocketConnection
      , allConnections ∷ Ref (HashMap Int AliveWebSocketConnection)
      )

type ServerReader = BaseReader
      ( configuration ∷ Configuration
      , session ∷ Session
      )

type BaseEffect r a = Run (READER r + EXCEPT ResponseError + AFF + EFFECT + ()) a

type DatabaseEffect a = Run (EXCEPT PgError + AFF + ()) a

type ServerEffect a = BaseEffect ServerReader a

type WebSocketEffect = BaseEffect WebSocketReader Unit

type StorageDetails =
      { authenticationKey ∷ String
      , accountAuthorizationToken ∷ Maybe String
      , uploadAuthorizationToken ∷ Maybe String
      , uploadUrl ∷ Maybe String
      , apiUrl ∷ Maybe String
      }

derive instance Newtype Html _

instance EncodeResponse Html where
      encodeResponse (PR.Response { status, headers, body: Html contents }) = pure $ PR.Response
            { headers: PH.setIfNotDefined "content-type" html headers
            , body: PR.StringBody contents
            , status
            }
