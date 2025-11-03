module Server.Response where

import Prelude

import Data.Maybe (Maybe(..))
import Run.Except as RE
import Server.Effect (BaseEffect)
import Shared.ResponseError (ResponseError(..))

throwInternalError ∷ ∀ r whatever. String → BaseEffect r whatever
throwInternalError reason = RE.throw $ InternalError { reason, context: Nothing }

throwBadRequest ∷ ∀ r whatever. String → BaseEffect r whatever
throwBadRequest reason = RE.throw $ BadRequest { reason }