module Shared.Common where

import Control.Monad.Error.Class(class MonadThrow)
import Effect.Exception(Error)
import Prelude
import Control.Monad.Error.Class as EC
import Effect.Exception as EE

parseJSONError :: forall a b. MonadThrow Error a => String -> a b
parseJSONError = EC.throwError <<< EE.error <<< ("Could not parse json: " <> _)