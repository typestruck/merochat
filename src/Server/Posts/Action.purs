module Server.Posts.Action where

import Prelude

import Data.Array as DA
import Data.Maybe as DM
import Data.Nullable as DN
import Data.String (Pattern(..))
import Data.String as DS
import Run.Except as RE
import Server.Effect (ServerEffect)
import Server.Im.Database.Permission as SIDPP
import Server.Posts.Database as SPD
import Server.Sanitize as SS
import Shared.Content (Content(..))
import Shared.Markdown (Token(..))
import Shared.Markdown as SM
import Shared.Options.Post (maxPostCharacters)
import Shared.Post (Post, PostPayload)
import Shared.Privilege (Privilege(..))
import Shared.ResponseError (ResponseError(..))

posts ∷ Int → Int → ServerEffect (Array Post)
posts loggedUserId userId = SPD.presentPosts loggedUserId userId

post ∷ Int → PostPayload → ServerEffect Unit
post loggedUserId payload = do
      privileges ← SIDPP.markdownPrivileges loggedUserId
      content ← case payload.content of
            Text text | canPostText privileges text → pure text
            Link link caption | canPostLink privileges link → pure ("[" <> DM.fromMaybe link caption <> "](" <> link <> ")")
            _ → pure ""
      let sanitized = SS.sanitize $ DS.trim content
      if not (DS.null sanitized) && DS.length sanitized <= maxPostCharacters then
            SPD.savePost loggedUserId sanitized
      else
            RE.throw $ BadRequest { reason: "cannot post post" }

canPostText ∷ ∀ r. Array { feature ∷ Privilege | r } → String → Boolean
canPostText privileges text = DA.any ((_ == PublishPosts) <<< _.feature) privileges && DA.all (not <<< isNotText) (SM.lexer text)
      where
      isNotText (Token child) = child."type" == "image" || child."type" == "link" || child."type" == "reflink" || DA.any isNotText (DM.fromMaybe [] $ DN.toMaybe child.tokens)

canPostLink ∷ ∀ r. Array { feature ∷ Privilege | r } → String → Boolean
canPostLink privileges link = DA.any ((_ == SendLinks) <<< _.feature) privileges && DS.contains (Pattern ".") link && DA.all (not <<< isNotImage) (SM.lexer link)
      where
      isNotImage (Token child) = child."type" == "image" || DA.any isNotImage (DM.fromMaybe [] $ DN.toMaybe child.tokens)
