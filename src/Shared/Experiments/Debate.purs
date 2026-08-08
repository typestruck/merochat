module Shared.Experiments.Debate where

import Prelude hiding (join)

import Data.Array ((:))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Experiments.Types (DebateSection(..), ExperimentsMessage(..), ExperimentsModel, PaperPlaneSection(..), PaperPlaneStatus(..))
import Shared.Options.Debate (maxStatementCharacters, maxTopicCharacters)

view ∷ ExperimentsModel → Html ExperimentsMessage
view model = HE.div [ HA.class' "paper-plane duller" ]
      [ HE.div [ HA.class' "green-tab" ]
              [ HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": model.debate.section == ShowNewDebate }, HA.onClick $ ToggleDebateSection ShowNewDebate ] [ HE.text "New" ]
              , HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": model.debate.section == ShowMine }, HA.onClick $ ToggleDebateSection ShowMine ] [ HE.text "Mine" ]
              , HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": model.debate.section == ShowJoin }, HA.onClick $ ToggleDebateSection ShowJoin ] [ HE.text "Join" ]
              , HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": model.debate.section == ShowPublic }, HA.onClick $ ToggleDebateSection ShowPublic ] [ HE.text "Public" ]
              ]
      , case model.debate.section of
              ShowNewDebate → new model
              ShowMine → mine model
              ShowJoin → join model
              ShowPublic → public model
      ]

new ∷ ExperimentsModel → Html ExperimentsMessage
new model =
      HE.div [ HA.class' "new-throw" ]
            [ HE.label [] [ HE.text "Topic for debate" ]
            , HE.select [ HA.class' "modal-select debate-topic", HA.onInput SetDebateTopic ]
                    [ HE.option [ HA.value "" ] [ HE.text "Select a topic" ]
                    , HE.option [ HA.value "Capitalism should be replaced by a different economic system" ] [ HE.text "Capitalism should be replaced by a different economic system" ]
                    , HE.option [ HA.value "Alcohol should be illegal" ] [ HE.text "Alcohol should be illegal" ]
                    , HE.option [ HA.value "All drugs should be decriminalized" ] [ HE.text "All drugs should be decriminalized" ]
                    , HE.option [ HA.value "Summer is the best season of the year" ] [ HE.text "Summer is the best season of the year" ]
                    , HE.option [ HA.value "God exists" ] [ HE.text "God exists" ]
                    , HE.option [ HA.value "Eating meat is unethical" ] [ HE.text "Eating meat is unethical" ]
                    , HE.option [ HA.value "Morality is objective" ] [ HE.text "Morality is objective" ]
                    , HE.option [ HA.value "Pineapple belongs on pizza" ] [ HE.text "Pineapple belongs on pizza" ]
                    , HE.option [ HA.value "Taylor Swift is a bigger pop star than Michael Jackson" ] [ HE.text "Taylor Swift is a bigger pop star than Michael Jackson" ]
                    , HE.option [ HA.value "Science can solve all of our problems" ] [ HE.text "Science can solve all of our problems" ]
                    , HE.option [ HA.value "Billionaires should not exist" ] [ HE.text "Billionaires should not exist" ]
                    , HE.option [ HA.value "Cats are better than dogs" ] [ HE.text "Cats are better than dogs" ]
                    , HE.option [ HA.value "Memes are a form of art" ] [ HE.text "Memes are a form of art" ]
                    , HE.option [ HA.value "AI will destroy the world" ] [ HE.text "AI will destroy the world" ]
                    , HE.option [ HA.value "Short form content is bad for mental health" ] [ HE.text "Short form content is bad for mental health" ]
                    , HE.option [ HA.value "Climate change is the most urgent scientific issue" ] [ HE.text "Climate change is the most urgent scientific issue" ]
                    , HE.option [ HA.value "Cars dependency is bad for society" ] [ HE.text "Cars dependency is bad for society" ]
                    , HE.option [ HA.value "All countries should have open borders" ] [ HE.text "All countries should have open borders" ]
                    , HE.option [ HA.value "We are living in a simulation" ] [ HE.text "We are living in a simulation" ]
                    , HE.option [ HA.value "Aliens exist" ] [ HE.text "Aliens exist" ]
                    , HE.option [ HA.value "Living forever would be a good thing" ] [ HE.text "Living forever would be a good thing" ]
                    ]
            , HE.label [] [ HE.text "Is your position pro or against?" ]
            , HE.select [ HA.class' "modal-select debate-topic", HA.onInput SetInFavor ]
                    [ HE.option [ HA.value "" ] [ HE.text $ "Select your position" ]
                    , HE.option [ HA.value "true" ] [ HE.text $ "Yes, I agree with: " <> DM.fromMaybe "" model.debate.topic ]
                    , HE.option [ HA.value "false" ] [ HE.text $ "No, I DO NOT agree with: " <> DM.fromMaybe "" model.debate.topic ]
                    ]

            , HE.div [ HA.class' "arguing debate-topic" ]
                    [ HE.text $ case model.debate.inFavor of
                            Nothing → ""
                            Just b → "You will be arguing" <> (if b then " for " else " against: ") <> DM.fromMaybe "" model.debate.topic
                    ]

            , if model.debate.loading then
                    HE.div' [ HA.class' "loading" ]
              else
                    HE.input
                          [ HA.type' "button"
                          , HA.onClick StartDebate
                          , HA.disabled (DM.isNothing model.debate.topic || DM.isNothing model.debate.statement)
                          , HA.class' "green-button"
                          , HA.value "Start debate"
                          ]
            ]

mine model = HE.div [] [ HE.text "Currently unavailable" ]

join model = HE.div [] [ HE.text "Currently unavailable" ]

public model = HE.div [] [ HE.text "Currently unavailable" ]