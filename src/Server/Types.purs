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
import Data.Maybe (Maybe)
import Database.PostgreSQL (class ToSQLValue, Pool, class FromSQLValue)
import Foreign as F
import HTTPure (Response)
import Run (AFF, Run, EFFECT)
import Run.Except (EXCEPT)
import Run.Reader (READER)

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

type ServerReader = {
        configuration :: Configuration,
        session :: Session,
        pool :: Pool
}

--needs logging strategy

type ServerEffect a = Run (
        reader :: READER ServerReader,
        except :: EXCEPT ResponseError,
        aff :: AFF,
        effect :: EFFECT
) a

type ResponseEffect = ServerEffect Response

data BenderAction = Name | Description

instance benderActionShow :: Show BenderAction where
        show Name = "name"
        show Description = "description"
