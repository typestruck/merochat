module Server.Experiments.Action where

import Prelude

import Data.Array as DA
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as DAN
import Debug (spy)
import Server.Effect (ServerEffect)
import Server.Experiments.Database as SED
import Server.Experiments.Database.Flat (FlatQuestion)
import Shared.Experiments.Types (Question, Match)
import Shared.Options.Doppelganger (changelogEntry, totalQuestions)
import Shared.Unsafe as SU

experiments ∷ Int → ServerEffect _
experiments loggedUserId = do
      list ← SED.fetchExperiments
      user ← SED.fetchExperimentUser loggedUserId
      count ← SED.fetchAnswerCount loggedUserId
      pure { experiments: list, user, completedDoppelganger: count == totalQuestions }

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