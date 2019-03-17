-- | Types common to all modules
module Server.Types where

import Data.Int53

import Database.PostgreSQL (Pool(..))
import HTTPure (ResponseM)
import Run (AFF, Run(..))
import Data.Maybe(Maybe(..))
import Run.Reader (READER)
import Run.State (STATE)
import Data.Generic.Rep (class Generic)

newtype Configuration = Configuration { port :: Int, development :: Boolean }

derive instance genericConfiguration :: Generic Configuration _

type Session = { user :: Maybe Int53 }

type ServerState = { session :: Session }

type ServerReader = { configuration :: Configuration, pool :: Pool }

type ServerEffect a = Run (state :: STATE ServerState, read :: READER ServerReader, aff :: AFF) a

type ResponseEffect = ServerEffect ResponseM
