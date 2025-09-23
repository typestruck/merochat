module Server.Posts.Action where

import Prelude

import Data.Array as DA
import Data.String as DS
import Run.Except as RE
import Server.Effect (ServerEffect)
import Server.Im.Database.Permission as SIDPP
import Server.Posts.Database as SPD
import Server.Sanitize as SS
import Shared.Post (Post, PostPayload)
import Shared.Privilege (Privilege(..))
import Shared.ResponseError (ResponseError(..))

posts ∷ Int → Int → ServerEffect (Array Post)
posts loggedUserId userId = SPD.presentPosts loggedUserId userId

post ∷ Int → PostPayload → ServerEffect Unit
post loggedUserId payload = do
      privileges ← SIDPP.markdownPrivileges loggedUserId
      let
            sanitized = payload
                  { content = SS.sanitize payload.content
                  }
      if DA.any ((_ == Posts) <<< _.feature) privileges || DS.null sanitized.content then
            SPD.savePost loggedUserId sanitized
      else
            RE.throw $ BadRequest { reason: "cannot post post" }

