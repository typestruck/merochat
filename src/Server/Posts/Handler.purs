module Server.Posts.Handler where

import Prelude
import Server.Effect
import Shared.Im.Types

import Data.Maybe (Maybe)
import Data.Newtype as DN
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
posts request = SPA.posts request.guards.loggedUserId request.query.poster

post ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ PostPayload } → ServerEffect { id ∷ Int }
post request = SPA.post request.guards.loggedUserId request.body

seen ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { poster ∷ Int, id ∷ Int } } → ServerEffect Empty
seen request = do
      SPA.markSeen request.guards.loggedUserId request.body.poster request.body.id
      pure Empty

