module Server.Feedback.Handler where

import Prelude
import Server.Types

import Data.Maybe (Maybe)
import Effect.Class (liftEffect)
import Server.Feedback.Action as SFA
import Server.Feedback.Template as SFT
import Server.Ok (Ok, ok)

feedback ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect String
feedback _ = liftEffect SFT.template

send ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { comments ∷ String, screenshot ∷ Maybe String } } → ServerEffect Ok
send { guards: { loggedUserId }, body: { comments, screenshot } } = do
      SFA.sendFeedback loggedUserId comments screenshot
      pure ok
