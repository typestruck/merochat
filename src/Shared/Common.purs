module Shared.Common where

import Control.Monad.Error.Class(class MonadThrow)
import Effect.Exception(Error)

parseJSONError :: forall a b. MonadThrow Error a => String -> a b
parseJSONError = EC.throwError <<< EE.error <<< ("Could not parse json: " <> _)