-- | Types common to all server side modules
module Server.Types where

import Prelude
import Shared.Types

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode as DAD
import Data.Enum (class BoundedEnum, Cardinality(..), class Enum)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype as DN
import Data.String as DS
import Data.String.Read (class Read)
import Database.PostgreSQL (Pool)
import Effect.Ref (Ref)
import HTTPure (Response)
import Payload.ContentType (html)
import Payload.Headers as PH
import Payload.ResponseTypes as PR
import Payload.Server.Response (class EncodeResponse)
import Run (AFF, Run, EFFECT)
import Run.Except (EXCEPT)
import Run.Reader (READER)
import Server.WebSocket (WebSocketConnection)

type ProfileUserEdition = {
      id :: String,
      avatar :: Maybe String,
      name :: String,
      headline :: String,
      description :: String,
      gender :: Maybe Gender,
      country :: Maybe PrimaryKey,
      tags :: Array String,
      birthday :: Maybe MDate
}

type Configuration = {
      port :: Int,
      development :: Boolean,
      captchaSecret :: String,
      tokenSecret :: String,
      salt :: String,
      emailHost :: String,
      emailUser :: String,
      emailPassword :: String
}

newtype CaptchaResponse = CaptchaResponse {
      success :: Boolean
}

instance decodeCaptchaResponse :: DecodeJson CaptchaResponse where
      decodeJson json = do
            object <- DAD.decodeJson json
            success <- DAD.getField object "success"
            pure $ CaptchaResponse { success }

type Session = {
      userID :: Maybe PrimaryKey
}

type BaseReader extension = {
      pool :: Pool |
      extension
}

type WebSocketReader = BaseReader (
      sessionUserID :: PrimaryKey,
      connection :: WebSocketConnection,
      allConnections:: Ref (Map PrimaryKey WebSocketConnection)
)

type ServerReader = BaseReader (
      configuration :: Configuration,
      session :: Session
)

type BaseEffect r a = Run (
      reader :: READER r,
      except :: EXCEPT ResponseError,
      aff :: AFF,
      effect :: EFFECT
) a

type ServerEffect a = BaseEffect ServerReader a

type ResponseEffect = BaseEffect ServerReader Response

type WebSocketEffect = BaseEffect WebSocketReader Unit

data BenderAction = Name | Description

instance benderActionShow :: Show BenderAction where
      show Name = "name"
      show Description = "description"

data ContentType = JSON | JS | GIF | JPEG | PNG | CSS | HTML | OctetStream

instance contentTypeShow :: Show ContentType where
      show JSON = "application/json"
      show JS = "application/javascript"
      show GIF = "image/gif"
      show JPEG = "image/jpeg"
      show PNG = "image/png"
      show CSS = "text/css"
      show HTML = "text/html"
      show _ = "application/octet-stream"

-- match both on file extension or content type
instance contentTypeRead :: Read ContentType where
      read v =
            Just $
                  if value == ".json" || value == show JSON then JSON
                   else if value == ".js" || value == show JS then JS
                   else if value == ".gif" || value == show GIF then GIF
                   else if value == ".jpeg" || value == ".jpg" || value == show JPEG then JPEG
                   else if value == ".png" || value == show PNG then PNG
                   else if value == ".css" || value == show CSS then CSS
                   else if value == ".html" || value == show HTML then HTML
                   else OctetStream
            where value = DS.trim $ DS.toLower v

derive instance eqBenderAction :: Eq BenderAction

--thats a lot of work...
instance ordBenderAction :: Ord BenderAction where
      compare Name Description = LT
      compare Description Name = GT
      compare _ _ = EQ

instance boundedBenderAction :: Bounded BenderAction where
      bottom = Name
      top = Description

instance boundedEnumBenderAction :: BoundedEnum BenderAction where
      cardinality = Cardinality 1

      fromEnum Name = 0
      fromEnum Description = 1

      toEnum 0 = Just Name
      toEnum 1 = Just Description
      toEnum _ = Nothing

instance enumBenderAction :: Enum BenderAction where
      succ Name = Just Description
      succ Description = Nothing

      pred Name = Nothing
      pred Description = Just Name

-------new

type AOk = Record ()

newtype Html = Html String

derive instance newtypeHtml :: Newtype Html _

instance encodeResponseHtml :: EncodeResponse Html where
      encodeResponse (PR.Response { status, headers, body: Html contents }) = pure $ PR.Response {
            headers: PH.setIfNotDefined "content-type" html headers,
            body: PR.StringBody contents,
            status
      }
