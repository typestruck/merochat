module Server.Response where

import Prelude
import Server.Types

import Effect (Effect)
import Run as R
import Run.Except as RE
import Shared.Types

serveTemplate :: Effect String -> ServerEffect Html
serveTemplate template = do
      contents <- R.liftEffect template
      pure $ Html contents

throwInternalError :: forall whatever. String -> ServerEffect whatever
throwInternalError reason = RE.throw $ InternalError { reason }

throwBadRequest :: forall r whatever. String -> BaseEffect r whatever
throwBadRequest reason = RE.throw $ BadRequest { reason }