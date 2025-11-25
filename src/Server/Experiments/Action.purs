module Server.Experiments.Action where

import Prelude

import Data.Array as DA
import Data.Array.NonEmpty as DAN
import Data.BigInt as BI
import Data.Maybe (Maybe(..))
import Debug (spy)
import Run.Except as RE
import Server.Database as SD
import Server.Effect (ServerEffect)
import Server.Experiments.Database as SED
import Shared.Experiments.Types (Match, PaperPlane, PaperPlaneStatus(..), Question)
import Shared.Options.Doppelganger (changelogEntry, totalQuestions)
import Shared.Options.PaperPlane (maxPaperPlanes)
import Shared.ResponseError (ResponseError(..))

experiments ∷ Int → ServerEffect _
experiments loggedUserId = do
      list ← SED.fetchExperiments
      user ← SED.fetchExperimentUser loggedUserId
      count ← SED.fetchAnswerCount loggedUserId
      thrown ← SED.fetchPaperPlanes loggedUserId
      flyingBy ← SED.fetchPaperPlanesFlying loggedUserId
      caught ← SED.fetchPaperPlanesCaught loggedUserId
      pure { experiments: list, user, completedDoppelganger: count == totalQuestions, thrown, flyingBy, caught }

buildQuestions ∷ Int → ServerEffect (Array Question)
buildQuestions loggedUserId = do
      flats ← SED.fetchQuestions loggedUserId
      pure <<< map build $ DA.groupBy sameQuestion flats
      where
      sameQuestion a b = a.questionId == b.questionId

      choiced c = { id: c.choiceId, description: c.choice, chosen: c.chosen }
      build records = let q = DAN.head records in { id: q.questionId, description: q.question, choices: DAN.toArray $ map choiced records }

fetchMatches ∷ Int → ServerEffect (Array Match)
fetchMatches loggedUserId = do
      matches ← SED.fetchMatches loggedUserId
      when (not $ DA.null matches) <<< SED.saveMatchChangelog loggedUserId changelogEntry $ map _.id matches
      pure matches

saveAnswer ∷ Int → Int → ServerEffect Unit
saveAnswer loggedUserId choice = SED.saveAnswer loggedUserId choice

throwPlane ∷ Int → String → ServerEffect { id ∷ Int }
throwPlane loggedUserId message = do
      c ← SED.countPaperPlanes loggedUserId
      when (c == Just (BI.fromInt maxPaperPlanes)) <<< RE.throw $ BadRequest { reason: "too many planes" }
      SED.savePlane loggedUserId message

catchPlane ∷ Int → Int → ServerEffect Unit
catchPlane loggedUserId id = SD.withTransaction $ \connection -> do
      SED.updatePlaneStatus connection loggedUserId id Caught
      found <- SED.fetchThrower connection loggedUserId id
      case found  of
            Just { thrower }-> SED.notifyPlaneCaught connection thrower "One your paper planes have been caught"
            _ -> pure unit

passPlane ∷ Int → Int → ServerEffect Unit
passPlane loggedUserId id = SED.updatePlaneBy loggedUserId id

flyingPlanes ∷ Int → ServerEffect (Array PaperPlane)
flyingPlanes loggedUserId = SED.fetchPaperPlanesFlying loggedUserId
