module Server.Posts.Handler where

import Prelude
import Server.Effect
import Shared.Im.Types

import Data.Maybe (Maybe)

import Data.Tuple (Tuple(..))
import Payload.ContentType (html)
import Payload.Headers as PH
import Payload.ResponseTypes (Empty(..), Response)
import Payload.Server.Response as PSR
import Server.Im.Action as SIA
import Server.Im.Template as SIT
import Server.Posts.Action as SPA
import Server.Response as SR
import Shared.Account (EmailPassword)
import Shared.Changelog (Changelog)
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Html (Html(..))
import Shared.Post (Post, PostPayload)

posts ∷ { guards ∷ { loggedUserId ∷ Int }, query ∷ { poster ∷ Int } } → ServerEffect (Array Post)
posts routes = SPA.posts routes.guards.loggedUserId routes.query.poster

post ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ PostPayload } → ServerEffect { id ∷ Int }
post routes = SPA.post routes.guards.loggedUserId routes.body

seen ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { poster ∷ Int, id ∷ Int } } → ServerEffect Empty
seen routes = do
      SPA.markSeen routes.guards.loggedUserId routes.body.poster routes.body.id
      pure Empty

