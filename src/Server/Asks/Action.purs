module Server.Asks.Action where

import Prelude

import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe as DM
import Data.Nullable as DN
import Data.String (Pattern(..))
import Data.String as DS
import Run.Except as RE
import Server.Asks.Database as SAD
import Server.Database.Privileges as SDP
import Server.Database.Privileges as SPD
import Server.Effect (ServerEffect)
import Server.File as SF
import Server.Sanitize as SS
import Shared.Content (Content(..))
import Shared.Markdown (Token(..))
import Shared.Markdown as SM
import Shared.Options.Ask (maxAskCharacters)
import Shared.Options.Post (maxPostCharacters)
import Shared.Post (Post, PostPayload)
import Shared.Privilege (Privilege(..))
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.ResponseError (ResponseError(..))

sendAsk ∷ Int → Int -> String → ServerEffect Boolean
sendAsk loggedUserId userId question = do
      let trimmed = DS.trim question
      when (DS.length trimmed > maxAskCharacters) <<< RE.throw $ BadRequest { reason : "question too long" }
      canSendAsk <- SPD.hasPrivilege loggedUserId SendAsks
      unless canSendAsk <<< RE.throw $ BadRequest { reason : "not enough karma" }
      allowedToAsk ← SAD.isAllowedToAsk loggedUserId userId
      when allowedToAsk $ SAD.saveAsk loggedUserId userId question
      pure allowedToAsk
