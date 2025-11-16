module Server.Experiments.Database where

import Droplet.Language
import Prelude hiding (join, not)
import Server.Database.Experiments
import Server.Database.Fields
import Server.Database.KarmaLeaderboard
import Server.Database.Privileges
import Shared.Experiments.Types

import Data.BigInt (BigInt)
import Data.BigInt as BI
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Server.Database as SD
import Server.Database.Changelogs (_action, _changed, _value, changelogs)
import Server.Database.DoppelgangerAnswers (_taker, doppelganger_answers)
import Server.Database.DoppelgangerChoices (_asked, _choice, doppelganger_choices)
import Server.Database.DoppelgangerQuestions (_question, doppelganger_questions)
import Server.Database.LastSeen (_who)
import Server.Database.Messages (_status)
import Server.Database.PaperPlanes (_message, _thrower, paper_planes)
import Server.Database.Recoveries (_created)
import Server.Effect (ServerEffect)
import Server.Experiments.Database.Flat (FlatQuestion)
import Shared.Changelog (ChangelogAction(..))
import Shared.Options.Doppelganger (totalQuestions)
import Shared.Privilege (Privilege)
import Shared.Unsafe as SU
import Type.Proxy (Proxy(..))

--could also order by popularity
fetchExperiments ∷ ServerEffect (Array ChatExperiment)
fetchExperiments = SD.query $ select (_id /\ _code /\ _name /\ _description) # from experiments # orderBy (_code # desc)

fetchExperimentUser ∷ Int → ServerEffect ChatExperimentUser
fetchExperimentUser loggedUserId = do
      record ← SD.single $ select (array_agg _feature # as _privileges) # from (join privileges karma_leaderboard # on (_ranker .=. loggedUserId .&&. _quantity .<=. _current_karma))
      pure <<< { privileges: _ } $ SU.fromJust do
            r ← record
            r.privileges

fetchAnswerCount ∷ Int → ServerEffect Int
fetchAnswerCount loggedUserId = do
      count ← SD.single $ select (count _id # as c) # from doppelganger_answers # wher (_taker .=. loggedUserId)
      pure $ SU.fromJust (count >>= BI.toInt <<< _.c)

fetchQuestions ∷ Int → ServerEffect (Array FlatQuestion)
fetchQuestions loggedUserId = SD.query $
      select ((q ... _id # as _questionId) /\ (c ... _id # as _choiceId) /\ (q ... _question # as _question) /\ _choice /\ ((exists (select (4 # as p) # from (doppelganger_answers # as a) # wher (_taker .=. loggedUserId .&&. (a ... _choice) .=. c ... _id))) # as _chosen))
            # from (join (doppelganger_choices # as c) (doppelganger_questions # as q) # on (q ... _id .=. c ... _asked))
            #
                  orderBy (q ... _id /\ random)

fetchMatches ∷ Int → ServerEffect (Array Match)
fetchMatches loggedUserId = SD.unsafeQuery "select * from (select u.id as id, name from doppelganger_answers a join doppelganger_answers b on a.choice = b.choice and a.taker = @loggedUserId and a.taker <> b.taker join users u on u.id = b.taker) a where not exists (select 1 from blocks where blocked = a.id and blocker = @loggedUserId or blocker = a.id and blocked = @loggedUserId) group by id, name having count(id) = @totalQuestions" { loggedUserId, totalQuestions }

saveMatchChangelog ∷ Int → String → Array Int → ServerEffect Unit
saveMatchChangelog loggedUserId description userIds = SD.execute $ insert # into changelogs (_changed /\ _description /\ _action /\ _value) # values (map vl userIds)
      where
      vl id = Just id /\ description /\ Just SendDoppelgangerMessage /\ Just loggedUserId

saveAnswer ∷ Int → Int → ServerEffect Unit
saveAnswer loggedUserId choice = SD.execute $ insert # into doppelganger_answers (_taker /\ _choice) # values (loggedUserId /\ choice)

savePlane ∷ Int → String → ServerEffect { id ∷ Int }
savePlane loggedUserId message = SU.fromJust <$> SD.single (insert # into paper_planes (_thrower /\ _message /\ _status) # values (loggedUserId /\ message /\ Flying) # returning _id)

countPaperPlanes ∷ Int → ServerEffect (Maybe BigInt)
countPaperPlanes loggedUserId = do
      count <- SD.single $ select (count _id # as c) # from paper_planes # wher (_thrower .=. loggedUserId .&&. _status .<>. Crashed)
      pure $ map _.c count

fetchPaperPlanes ∷ Int → ServerEffect (Array PaperPlane)
fetchPaperPlanes loggedUserId = SD.query $ select (_id /\ _message /\ _status) # from paper_planes # wher (_thrower .=. loggedUserId .&&. _status .<>. Crashed) # orderBy _created

q ∷ Proxy "q"
q = Proxy

a ∷ Proxy "a"
a = Proxy

_questionId ∷ Proxy "questionId"
_questionId = Proxy

_choiceId ∷ Proxy "choiceId"
_choiceId = Proxy

_chosen ∷ Proxy "chosen"
_chosen = Proxy