-- | Types common to all modules
module Server.Types where

import Prelude
import Shared.Types

import Control.Monad.Except as CME
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode as DAD
import Data.Bifunctor as DB
import Data.Generic.Rep (class Generic)
import Data.Int53 (Int53)
import Data.Int53 as DI
import Data.Map (Map)
import Data.Maybe (Maybe)
import Database.PostgreSQL (class ToSQLValue, Pool, class FromSQLValue)
import Effect.Ref (Ref)
import Foreign as F
import HTTPure (Response)
import Run (AFF, Run, EFFECT)
import Run.Except (EXCEPT)
import Run.Reader (READER)
import Server.WebSocket (WebSocketConnection)

newtype Configuration = Configuration {
        port :: Int,
        development :: Boolean,
        captchaSecret :: String,
        benderURL :: String,
        useBender :: Boolean,
        tokenSecretGET :: String,
        tokenSecretPOST :: String,
        salt :: String
}

derive instance genericConfiguration :: Generic Configuration _

newtype CaptchaResponse = CaptchaResponse {
        success :: Boolean
}

instance decodeCaptchaResponse :: DecodeJson CaptchaResponse where
        decodeJson json = do
                object <- DAD.decodeJson json
                success <- DAD.getField object "success"
                pure $ CaptchaResponse { success }

type Session = {
        userID :: Maybe Int53
}

type BaseReader extension = {
        configuration :: Configuration,
        pool :: Pool |
        extension
}

type WebSocketReader = BaseReader (
        allConnections:: Ref (Map Int53 WebSocketConnection)
)

type ServerReader = BaseReader (
        session :: Session
)

--needs logging strategy

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