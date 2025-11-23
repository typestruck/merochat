module Server.Experiments.Handler where

import Prelude
import Server.Effect (ServerEffect)

import Payload.ResponseTypes (Empty(..))
import Run as R
import Server.Experiments.Action as SEA
import Server.Experiments.Template as SET
import Shared.Experiments.Types (Match, PaperPlane, Question)
import Shared.Html (Html)

experiments ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect Html
experiments { guards: { loggedUserId } } = do
      payload ← SEA.experiments loggedUserId
      R.liftEffect $ SET.template payload

questions ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect (Array Question)
questions request = SEA.buildQuestions request.guards.loggedUserId

matches ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect (Array Match)
matches request = SEA.fetchMatches request.guards.loggedUserId

answer ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { choice ∷ Int } } → ServerEffect Empty
answer request = do
      SEA.saveAnswer request.guards.loggedUserId request.body.choice
      pure Empty

throw ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { message ∷ String } } → ServerEffect { id ∷ Int }
throw request = SEA.throwPlane request.guards.loggedUserId request.body.message

catch ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { id :: Int} } → ServerEffect Empty
catch request = do
      SEA.catchPlane request.guards.loggedUserId request.body.id
      pure Empty

pass ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { id :: Int} } → ServerEffect Empty
pass request = do
      SEA.passPlane request.guards.loggedUserId request.body.id
      pure Empty

flying ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect (Array PaperPlane)
flying request = SEA.flyingPlanes request.guards.loggedUserId

