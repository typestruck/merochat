module Server.Feedback.Handler where

import Prelude
import Server.Effect

import Data.Maybe (Maybe)
import Effect.Class (liftEffect)
import Payload.ResponseTypes (Empty(..))
import Server.Feedback.Action as SFA
import Server.Feedback.Template as SFT
import Shared.Html (Html)

feedback ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect Html
feedback _ = liftEffect SFT.template

send ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { comments ∷ String, screenshot ∷ Maybe String } } → ServerEffect Empty
send { guards: { loggedUserId }, body: { comments, screenshot } } = do
      SFA.sendFeedback loggedUserId comments screenshot
      pure Empty
