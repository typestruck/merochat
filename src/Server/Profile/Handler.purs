module Server.Profile.Handler where

import Prelude
import Server.Effect
import Shared.Profile.Types
import Shared.User

import Data.Maybe (Maybe(..))
import Payload.ResponseTypes (Empty(..))
import Run as R
import Server.Profile.Action as SPA
import Server.Profile.Database as SPD
import Server.Profile.Database.Flat as SPDF
import Server.Profile.Template as SPT
import Shared.Ask (Ask)
import Shared.Html (Html)
import Shared.Post (Post)
import Shared.Profile.Mode (ProfileMode)

profile ∷ { guards ∷ { loggedUserId ∷ Int }, query ∷ { mode ∷ ProfileMode } } → ServerEffect Html
profile { guards: { loggedUserId }, query } = do
      payload ← SPA.profile loggedUserId
      R.liftEffect $ SPT.template payload query.mode

generated ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { field ∷ What } } → ServerEffect String
generated { body } = SPA.generateField body.field

posts ∷ { guards ∷ { loggedUserId ∷ Int }, query ∷ { after ∷ Maybe Int } } → ServerEffect (Array Post)
posts routes = SPA.refreshPosts routes.guards.loggedUserId routes.query.after

asks ∷ { guards ∷ { loggedUserId ∷ Int }, query ∷ { after ∷ Maybe Int } } → ServerEffect (Array Ask)
asks routes = SPA.refreshAsks routes.guards.loggedUserId routes.query.after

answer ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { id ∷ Int, answer ∷ String } } → ServerEffect Empty
answer routes = do
      SPA.saveAnswer routes.guards.loggedUserId routes.body.id routes.body.answer
      pure Empty

save ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ SavedFields } → ServerEffect { avatar ∷ Maybe String }
save routes = SPA.save routes.guards.loggedUserId routes.body

ignore ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { id ∷ Int } } → ServerEffect Empty
ignore routes = do
      SPA.ignoreAsk routes.guards.loggedUserId routes.body.id
      pure Empty

report ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { id ∷ Int, userId ∷ Int } } → ServerEffect Empty
report routes = do
      SPA.reportAsk routes.guards.loggedUserId routes.body.id routes.body.userId
      pure Empty