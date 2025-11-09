module Client.Experiments.Update where

import Prelude
import Shared.Experiments.Types

import Client.Location as CCL
import Client.Network (request)
import Client.Network as CCN
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class as EC
import Flame (Update)
import Flame as F
import Flame.Subscription as FS
import Shared.Im.Types (RetryableRequest(..))
import Shared.Im.Types as SIT
import Shared.Modal (Modal(..), ScreenModal(..))
import Client.AppId (imAppId)
import Shared.Unsafe as SU

update ∷ Update ExperimentsModel ExperimentsMessage
update model =
      case _ of
            ToggleVisibility modal → model { visible = modal == ShowExperiments } /\ []
            ToggleSection section → F.noMessages model { section = section }
            RedirectKarma → model /\
                  [ do
                          liftEffect <<< FS.send imAppId <<< SIT.SpecialRequest <<< ToggleModal $ Screen ShowKarmaPrivileges
                          pure Nothing
                  ]
            UpdatePrivileges { privileges } → F.noMessages model { user { privileges = privileges } }
            ResumeQuestions → resumeQuestions model
            DisplayQuestions questions → displayQuestions questions model
            SelectChoice question choice → selectChoice question choice model
            AnswerQuestion → answerQuestion model
            AfterAnswerQuestion → afterAnswerQuestion model
            FetchMatches → fetchMatches model
            DisplayMatches matches → displayMatches matches model
            MessageDoppelganger userId → messageDoppelganger userId model

resumeQuestions ∷ ExperimentsModel → ExperimentsModel /\ (Array (Aff (Maybe ExperimentsMessage)))
resumeQuestions model = model /\ [ resume ]
      where
      resume = do
            questions ← CCN.silentResponse $ request.experiments.questions {}
            pure <<< Just $ DisplayQuestions questions

displayQuestions ∷ Array Question → ExperimentsModel → ExperimentsModel /\ (Array (Aff (Maybe ExperimentsMessage)))
displayQuestions questions model = model { section = ShowingDoppelganger ShowNextQuestion, doppelganger { questions = questions } } /\ []

selectChoice ∷ Int → Int → ExperimentsModel → ExperimentsModel /\ (Array (Aff (Maybe ExperimentsMessage)))
selectChoice question choice model = model { doppelganger = model.doppelganger { selectedChoice = Just { question, choice } } } /\ []

answerQuestion ∷ ExperimentsModel → ExperimentsModel /\ (Array (Aff (Maybe ExperimentsMessage)))
answerQuestion model = model { doppelganger = model.doppelganger { loading = true } } /\ [ answer ]
      where
      answer = do
            let selected = SU.fromJust model.doppelganger.selectedChoice
            void <<< CCN.silentResponse $ request.experiments.answer { body: { choice: selected.choice } }
            pure $ Just AfterAnswerQuestion

afterAnswerQuestion ∷ ExperimentsModel → ExperimentsModel /\ (Array (Aff (Maybe ExperimentsMessage)))
afterAnswerQuestion model =
      model
            { doppelganger = model.doppelganger
                    { loading = false
                    , selectedChoice = Nothing
                    , questions = updatedQuestions
                    }
            } /\ effects
      where
      updatedQuestions = map mark model.doppelganger.questions
      selected = SU.fromJust model.doppelganger.selectedChoice
      choose choice
            | choice.id == selected.choice = choice { chosen = true }
            | otherwise = choice
      mark question
            | question.id == selected.question = question { choices = map choose question.choices }
            | otherwise = question

      hasAnswer question = DA.any _.chosen question.choices
      effects
            | DA.all hasAnswer updatedQuestions = [ pure $ Just FetchMatches ]
            | otherwise = []

fetchMatches ∷ ExperimentsModel → ExperimentsModel /\ (Array (Aff (Maybe ExperimentsMessage)))
fetchMatches model = model /\ [ fetch ]
      where
      fetch = do
            response ← CCN.silentResponse $ request.experiments.matches {}
            pure <<< Just $ DisplayMatches response

displayMatches ∷ Array Match → ExperimentsModel → ExperimentsModel /\ (Array (Aff (Maybe ExperimentsMessage)))
displayMatches matches model = model { section = ShowingDoppelganger ShowMatches, doppelganger = model.doppelganger { matches = matches } } /\ []

messageDoppelganger ∷ Int → ExperimentsModel → ExperimentsModel /\ (Array (Aff (Maybe ExperimentsMessage)))
messageDoppelganger userId model = model /\ [ send ]
      where
      send = EC.liftEffect do
            FS.send imAppId $ SIT.MessageDoppelganger userId
            pure Nothing