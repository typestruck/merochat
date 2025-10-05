module Server.Experiments.Handler where

import Prelude
import Server.Effect

import Run as R
import Server.Experiments.Action as SEA
import Server.Experiments.Template as SET
import Server.Ok (Ok, ok)
import Shared.Experiments.Types (Question, Match)

experiments ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect String
experiments { guards: { loggedUserId } } = do
      payload ← SEA.experiments loggedUserId
      R.liftEffect $ SET.template payload

questions ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect (Array Question)
questions request = SEA.buildQuestions request.guards.loggedUserId

matches ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect (Array Match)
matches request = SEA.fetchMatches request.guards.loggedUserId

answer ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { choice ∷ Int } } → ServerEffect Ok
answer request = do
      SEA.saveAnswer request.guards.loggedUserId request.body.choice
      pure ok

