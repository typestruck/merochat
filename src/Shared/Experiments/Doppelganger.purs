module Shared.Experiments.Doppelganger where

import Prelude

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Experiments.Types (DoppelgangerSection(..), ExperimentsMessage(..), ExperimentsModel)
import Shared.Extra as SE

view ∷ ExperimentsModel → Html ExperimentsMessage
view model = HE.div [ HA.class' "word-chain duller" ]
      [ case model.doppelganger.section of
              ShowNextQuestion → showNextQuestion
              ShowMatches → showMatches
              _ →
                    if model.doppelganger.completed then
                          HE.button [ HA.onClick FetchMatches, HA.class' "green-button" ] [ HE.text $ "Check results" ]
                    else
                          HE.button [ HA.onClick ResumeQuestions, HA.class' "green-button" ] [ HE.text $ "Take quiz" ]
      ]
      where
      matchLink match = SE.a [ HA.title "Send message to user", HA.onClick $ MessageFromExperiment match.id ] [ HE.text match.name ]

      showMatches
            | DA.null model.doppelganger.matches = HE.div [] [ HE.text "No evil clones found. Yet." ]
            | otherwise = HE.div [ HA.class' "matches" ]
                    [ HE.div [] [ HE.text "You have evil clones!" ]
                    , HE.div [ HA.class' "lynx-inline" ]
                            [ HE.text "You and "
                            , HE.span [ HA.class' "lynx-inline" ] <<< DA.intersperse (HE.text ", ") $ map matchLink model.doppelganger.matches
                            , HE.text " have chosen the exact same 9 answers"
                            ]
                    ]

      showChoice questionId choice = SE.choice
            (model.doppelganger.selectedChoice == Just { question: questionId, choice: choice.id })
            (SelectChoice questionId choice.id)
            choice.description

      unaswered question = DA.all (not <<< _.chosen) question.choices
      showNextQuestion = case DA.head $ DA.filter unaswered model.doppelganger.questions of
            Nothing → HE.span_ [ HE.text "Searching..." ]
            Just question → HE.div [ HA.class' "question" ]
                  [ HE.label_ [ HE.text question.description ]
                  , HE.div [ HA.class' "choices" ] $ map (showChoice question.id) question.choices
                  , if model.doppelganger.loading then
                          HE.div' [ HA.class' "loading" ]
                    else
                          HE.button [ HA.disabled $ DM.isNothing model.doppelganger.selectedChoice, HA.onClick AnswerQuestion, HA.class' "green-button" ] [ HE.text $ "Save answer" ]
                  ]
