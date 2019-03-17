-- | Types common to all modules
module Server.Types where

import Data.Int53

import Database.PostgreSQL (Pool(..))
import HTTPure (Response)
import Run (AFF, Run(..), EFFECT)
import Data.Maybe(Maybe(..))
import Run.Reader (READER)
import Run.State (STATE)
import Data.Generic.Rep (class Generic)

newtype Configuration = Configuration { port :: Int, development :: Boolean }

derive instance genericConfiguration :: Generic Configuration _

type Session = { user :: Maybe Int53 }

type ServerState = { session :: Session }

type ServerReader = { configuration :: Configuration, pool :: Pool }

--needs error strategy as well as logging

type ServerEffect a = Run (reader :: READER ServerReader, state :: STATE ServerState, aff :: AFF, effect :: EFFECT) a

type ResponseEffect = ServerEffect Response
