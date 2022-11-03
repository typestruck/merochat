-- | Types common to all server side modules
module Server.Types where


import Data.Maybe (Maybe)
import Droplet.Driver (Pool)
import Run (AFF, Run, EFFECT)
import Run.Except (EXCEPT)
import Run.Reader (READER)
import Shared.ResponseError (ResponseError)
import Type.Row (type (+))

type Configuration =
      { port ∷ Int
      , captchaSecret ∷ String
      , adminSecret :: String
      , storageApplicationKey ∷ String
      , storageApplicationKeyId ∷ String
      , tokenSecret ∷ String
      , salt ∷ String
      , databaseHost ∷ Maybe String
      , emailHost ∷ String
      , emailUser ∷ String
      , emailPassword ∷ String
      }

type BaseReader extension =
      { pool ∷ Pool
      | extension
      }

type ServerReader = BaseReader
      ( configuration ∷ Configuration
      )

type BaseEffect r a = Run (READER r + EXCEPT ResponseError + AFF + EFFECT + ()) a

type ServerEffect a = BaseEffect ServerReader a
