module Server.Asks.Handler where

import Prelude
import Server.Effect
import Shared.Im.Types

import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Payload.ContentType (html)
import Payload.Headers as PH
import Payload.ResponseTypes (Empty(..), Response)
import Payload.Server.Response as PSR
import Server.Asks.Action as SAA
import Server.Im.Action as SIA
import Server.Im.Template as SIT
import Server.Posts.Action as SPA
import Server.Response as SR
import Shared.Account (EmailPassword)
import Shared.Ask (Ask)
import Shared.Changelog (Changelog)
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Html (Html(..))

asks ∷ { guards ∷ { loggedUserId ∷ Int }, query ∷ { answerer ∷ Int } } → ServerEffect (Array Ask)
asks routes = SAA.presentAsks routes.query.answerer

post ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { userId ∷ Int, question ∷ String } } → ServerEffect { allowed ∷ Boolean }
post routes = do
      allowed ← SAA.sendAsk routes.guards.loggedUserId routes.body.userId routes.body.question
      pure { allowed }

