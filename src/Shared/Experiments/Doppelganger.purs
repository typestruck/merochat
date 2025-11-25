module Shared.Experiments.Doppelganger where

import Prelude

import Client.Dom as CCD
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Experiments.Types (DoppelgangerSection(..), ExperimentsMessage(..), ExperimentsModel)
import Shared.Unsafe as SU
import Web.HTML.HTMLInputElement as WHHI

view ∷ ExperimentsModel → Html ExperimentsMessage
view model = HE.div [ HA.class' "word-chain duller" ]
      [ case model.doppelganger.section of
              ShowNextQuestion → showNextQuestion
              ShowMatches → showMatches
              _ →
                    if model.doppelganger.completed then
                          HE.input [ HA.type' "button", HA.onClick FetchMatches, HA.class' "green-button", HA.value "Check results" ]
                    else
                          HE.input [ HA.type' "button", HA.onClick ResumeQuestions, HA.class' "green-button", HA.value "Take quiz" ]
      ]
      where
      matchLink match = HE.a [ HA.title "Send message to user", HA.onClick $ MessageFromExperiment match.id ] [ HE.text match.name ]

      showMatches
            | DA.null model.doppelganger.matches = HE.div [] [ HE.text "No evil clones found. Yet." ]
            | otherwise = HE.div [ HA.class' "matches" ]
                    [ HE.div [] [ HE.text "You have evil clones!" ]
                    , HE.div []
                            [ HE.text "You and "
                            , HE.span_ <<< DA.intersperse (HE.text ", ") $ map matchLink model.doppelganger.matches
                            , HE.text " have chosen the exact same 9 answers"
                            ]
                    ]

      showChoice questionId choice = HE.div [ HA.class' "choice-option" ]
            [ --i think it is a bug on flame, input checked property for that position in the group is not cleared
              HE.managed { createNode: createOption, updateNode: resetChecked } [ HA.checked false, HA.autocomplete "off", HA.type' "radio", HA.name $ "choices" <> show questionId, HA.value $ show choice.id, HA.id $ show choice.id, HA.onChange $ SelectChoice questionId choice.id ] model.doppelganger.selectedChoice
            , HE.label [ HA.for $ show choice.id ] [ HE.text choice.description ]
            ]

      unaswered question = DA.all (not <<< _.chosen) question.choices
      showNextQuestion = case DA.head $ DA.filter unaswered model.doppelganger.questions of
            Nothing → HE.span_ [ HE.text "Searching..." ]
            Just question → HE.div [ HA.class' "question" ]
                  [ HE.label_ [ HE.text question.description ]
                  , HE.div [ HA.class' "choices" ] $ map (showChoice question.id) question.choices
                  , if model.doppelganger.loading then
                          HE.div' [ HA.class' "loading" ]
                    else
                          HE.input [ HA.disabled $ DM.isNothing model.doppelganger.selectedChoice, HA.type' "button", HA.onClick AnswerQuestion, HA.class' "green-button", HA.value "Save answer" ]
                  ]

      createOption _ = do
            element ← SU.fromJust <<< WHHI.fromElement <$> CCD.createElement "input"
            WHHI.setType "radio" element
            pure $ WHHI.toNode element

      resetChecked node _ selectedChoice = do
            let element = SU.fromJust $ WHHI.fromNode node
            id ← WHHI.value element
            WHHI.setChecked (id == show (DM.maybe 0 (_.choice) selectedChoice)) element
            pure node
