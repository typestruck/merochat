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

profile ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect Html
profile { guards: { loggedUserId } } = do
      payload <- SPA.profile loggedUserId
      R.liftEffect $ SPT.template payload

generated ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { field ∷ What } } → ServerEffect String
generated { body } = SPA.generateField body.field

posts ∷ { guards ∷ { loggedUserId ∷ Int }, query ∷ { after ∷ Maybe Int } } → ServerEffect (Array Post)
posts request = SPA.refreshPosts request.guards.loggedUserId request.query.after

asks ∷ { guards ∷ { loggedUserId ∷ Int }, query ∷ { after ∷ Maybe Int } } → ServerEffect (Array Ask)
asks request = SPA.refreshAsks request.guards.loggedUserId request.query.after

answer ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ {id :: Int, answer :: String} } → ServerEffect Empty
answer request = do
      SPA.saveAnswer request.guards.loggedUserId request.body.id request.body.answer
      pure Empty

save ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ SavedFields } → ServerEffect { avatar ∷ Maybe String }
save request = SPA.save request.guards.loggedUserId request.body