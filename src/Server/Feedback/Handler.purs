module Server.Feedback.Handler where

import Prelude
import Server.Types

import Effect.Class (liftEffect)
import Server.Feedback.Template as SFT

feedback ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect String
feedback _ = liftEffect SFT.template
