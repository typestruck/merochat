-- | Types common to all modules
module Server.Types where

import Data.Int53
import Shared.Types

import Data.Date (Date)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Database.PostgreSQL (class ToSQLValue, Pool(..))
import Foreign as F
import HTTPure (Response)
import Run (AFF, Run(..), EFFECT)
import Run.Except (EXCEPT)
import Run.Reader (READER)
import Run.State (STATE)

newtype Configuration = Configuration {
	port :: Int,
	development :: Boolean,
	captchaSecret :: String
}

derive instance genericConfiguration :: Generic Configuration _

type Session = { user :: Maybe Int53 }

type ServerState = { session :: Session }

type ServerReader = {
	configuration :: Configuration,
	pool :: Pool
}

--needs logging strategy

type ServerEffect a = Run (
	reader :: READER ServerReader,
	state :: STATE ServerState,
	except :: EXCEPT ResponseError,
	aff :: AFF,
	effect :: EFFECT
) a

type ResponseEffect = ServerEffect Response

data By = ID PrimaryKey | Email String

newtype PrimaryKey = PrimaryKey Int53

instance primaryKeySQLValue :: ToSQLValue PrimaryKey where
	toSQLValue (PrimaryKey integer) = F.unsafeToForeign integer