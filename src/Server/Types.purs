-- | Types common to all modules
module Server.Types where

import Data.Int53
import Shared.Types

import Data.Date (Date)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Database.PostgreSQL (Pool(..))
import HTTPure (Response)
import Run (AFF, Run(..), EFFECT)
import Run.Except (EXCEPT)
import Run.Reader (READER)
import Run.State (STATE)

newtype Configuration = Configuration {
	port :: Int,
	development :: Boolean
}

derive instance genericConfiguration :: Generic Configuration _

type Session = { user :: Maybe Int53 }

type ServerState = { session :: Session }

type ServerReader = {
	configuration :: Configuration,
	pool :: Pool
}

--needs error strategy as well as logging

type ServerEffect a = Run (reader :: READER ServerReader, state :: STATE ServerState, except :: EXCEPT ResponseError, aff :: AFF, effect :: EFFECT) a

type ResponseEffect = ServerEffect Response

data By = ID Int53 | Email String | Name String