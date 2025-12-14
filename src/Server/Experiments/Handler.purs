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
questions routes = SEA.buildQuestions routes.guards.loggedUserId

matches ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect (Array Match)
matches routes = SEA.fetchMatches routes.guards.loggedUserId

answer ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { choice ∷ Int } } → ServerEffect Empty
answer routes = do
      SEA.saveAnswer routes.guards.loggedUserId routes.body.choice
      pure Empty

throw ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { message ∷ String } } → ServerEffect { id ∷ Int }
throw routes = SEA.throwPlane routes.guards.loggedUserId routes.body.message

catch ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { id :: Int} } → ServerEffect Empty
catch routes = do
      SEA.catchPlane routes.guards.loggedUserId routes.body.id
      pure Empty

pass ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { id :: Int} } → ServerEffect Empty
pass routes = do
      SEA.passPlane routes.guards.loggedUserId routes.body.id
      pure Empty

flying ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect (Array PaperPlane)
flying routes = SEA.flyingPlanes routes.guards.loggedUserId

