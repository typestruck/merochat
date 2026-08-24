module Server.Praise.Action where

import Prelude

import Run.Except as RE
import Server.Database as SD
import Server.Database.Privileges as SDP
import Server.Effect (ServerEffect)
import Server.Praise.Database as SPD
import Shared.Praise (Praise, PraisedFor, PraiseDisplay)
import Shared.Privilege (Privilege(..))
import Shared.ResponseError (ResponseError(..))

presentPraise ∷ Int → Int → ServerEffect PraiseDisplay
presentPraise loggedUserId userId = do
      praise ← SPD.presentPraise userId
      isAllowedToPraise ← SPD.isAllowedToPraise loggedUserId userId
      pure { praise, alreadyPraised: not isAllowedToPraise }

savePraise ∷ Int → Int → Array PraisedFor → ServerEffect Boolean
savePraise loggedUserId userId for = do
      canSavePraise ← SDP.hasPrivilege loggedUserId SendPraise
      unless canSavePraise <<< RE.throw $ BadRequest { reason: "not enough karma" }
      allowedToPraise ← SPD.isAllowedToPraise loggedUserId userId
      when allowedToPraise <<< SD.withTransaction $ \connection → do
            SPD.savePraise connection loggedUserId userId for
            SPD.notifyPraise connection userId
      pure allowedToPraise
