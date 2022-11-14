module Server.Response where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Run as R
import Run.Except as RE
import Server.Effect (BaseEffect, ServerEffect)
import Shared.Html (Html(..))
import Shared.ResponseError (ResponseError(..))

serveTemplate ∷ Effect String → ServerEffect Html
serveTemplate template = do
      contents ← R.liftEffect template
      pure $ Html contents

throwInternalError ∷ ∀ r whatever. String → BaseEffect r whatever
throwInternalError reason = RE.throw $ InternalError { reason, context: Nothing }

throwBadRequest ∷ ∀ r whatever. String → BaseEffect r whatever
throwBadRequest reason = RE.throw $ BadRequest { reason }