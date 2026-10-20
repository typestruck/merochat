module Shared.Experiments.Debate where

import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Experiments.Types (DebateSection(..), ExperimentsMessage(..), ExperimentsModel)
import Shared.Extra as SE
import Shared.Options.Debate (maxStatementCharacters)
import Prelude hiding (join)

view ∷ ExperimentsModel → Html ExperimentsMessage
view model = HE.div [ HA.class' "paper-plane duller" ]
      [ HE.span [ HA.class' "debate-how", HA.onClick ToggleFormat ] [ HE.text "Click here to check the debate format" ]
      , HE.div [ HA.class' { "debate-format": true, hidden: not model.debate.showFormat } ]
              [ HE.text "To start a debate, select a topic and pick a side (either pro or against). Someone else will join the debate arguing of the opposite site. Debates are divided into rounds: "
              , HE.text "\n"
              , HE.text "- Every round has a statement from both sides"
              , HE.text "\n"
              , HE.text "- Under each statement a maximum of six clarifying questions are allowed. Each question is allowed one response"
              , HE.text "\n"
              , HE.text "- A round ends when either all questions have been answered or both parties agreed to move into the next round"
              , HE.text "\n"
              , HE.text "There is no limit to the number of rounds. At any start either participant may decide to end the debate"
              , HE.text "\n"
              , HE.text "Debates may be either private or public. Public debates are visible to users besides the arguing parties, and users may vote on who they think argued better"
              ]
      , HE.div [ HA.class' "green-tab lynx-inline" ]
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
            , HE.div [ HA.class' "modal-select debate-topic" ]
                    [ SE.choice (DM.isNothing model.debate.topic) (SetDebateTopic "") "Select a topic"
                    , SE.choice (model.debate.topic == Just "Capitalism should be replaced by a different economic system") (SetDebateTopic "Capitalism should be replaced by a different economic system") "Capitalism should be replaced by a different economic system"
                    , SE.choice (model.debate.topic == Just "Alcohol should be illegal") (SetDebateTopic "Alcohol should be illegal") "Alcohol should be illegal"
                    , SE.choice (model.debate.topic == Just "All drugs should be decriminalized") (SetDebateTopic "All drugs should be decriminalized") "All drugs should be decriminalized"
                    , SE.choice (model.debate.topic == Just "Summer is the best season of the year") (SetDebateTopic "Summer is the best season of the year") "Summer is the best season of the year"
                    , SE.choice (model.debate.topic == Just "God exists") (SetDebateTopic "God exists") "God exists"
                    , SE.choice (model.debate.topic == Just "Eating meat is unethical") (SetDebateTopic "Eating meat is unethical") "Eating meat is unethical"
                    , SE.choice (model.debate.topic == Just "Morality is objective") (SetDebateTopic "Morality is objective") "Morality is objective"
                    , SE.choice (model.debate.topic == Just "Pineapple belongs on pizza") (SetDebateTopic "Pineapple belongs on pizza") "Pineapple belongs on pizza"
                    , SE.choice (model.debate.topic == Just "Taylor Swift is a bigger pop star than Michael Jackson") (SetDebateTopic "Taylor Swift is a bigger pop star than Michael Jackson") "Taylor Swift is a bigger pop star than Michael Jackson"
                    , SE.choice (model.debate.topic == Just "Science can solve all of our problems") (SetDebateTopic "Science can solve all of our problems") "Science can solve all of our problems"
                    , SE.choice (model.debate.topic == Just "Billionaires should not exist") (SetDebateTopic "Billionaires should not exist") "Billionaires should not exist"
                    , SE.choice (model.debate.topic == Just "Cats are better than dogs") (SetDebateTopic "Cats are better than dogs") "Cats are better than dogs"
                    , SE.choice (model.debate.topic == Just "Memes are a form of art") (SetDebateTopic "Memes are a form of art") "Memes are a form of art"
                    , SE.choice (model.debate.topic == Just "AI will destroy the world") (SetDebateTopic "AI will destroy the world") "AI will destroy the world"
                    , SE.choice (model.debate.topic == Just "Short form content is bad for mental health") (SetDebateTopic "Short form content is bad for mental health") "Short form content is bad for mental health"
                    , SE.choice (model.debate.topic == Just "Climate change is the most urgent scientific issue") (SetDebateTopic "Climate change is the most urgent scientific issue") "Climate change is the most urgent scientific issue"
                    , SE.choice (model.debate.topic == Just "Cars dependency is bad for society") (SetDebateTopic "Cars dependency is bad for society") "Cars dependency is bad for society"
                    , SE.choice (model.debate.topic == Just "All countries should have open borders") (SetDebateTopic "All countries should have open borders") "All countries should have open borders"
                    , SE.choice (model.debate.topic == Just "We are living in a simulation") (SetDebateTopic "We are living in a simulation") "We are living in a simulation"
                    , SE.choice (model.debate.topic == Just "Aliens exist") (SetDebateTopic "Aliens exist") "Aliens exist"
                    , SE.choice (model.debate.topic == Just "Living forever would be a good thing") (SetDebateTopic "Living forever would be a good thing") "Living forever would be a good thing"
                    ]
            , HE.label [] [ HE.text "Is your position pro or against?" ]
            , HE.div [ HA.class' "modal-select debate-topic" ]
                    [ HE.text "Select your position"
                    , SE.choice (model.debate.inFavor == Just true) (SetInFavor "true") $ "Yes, I agree with: " <> DM.fromMaybe "" model.debate.topic
                    , SE.choice (model.debate.inFavor == Just false) (SetInFavor "false") $ "No, I DO NOT agree with: " <> DM.fromMaybe "" model.debate.topic
                    ]

            , HE.div [ HA.class' { "arguing debate-topic": true, hidden: DM.isNothing model.debate.inFavor } ]
                    [ HE.text $ "Please write your initial statement" <> (DM.maybe "" (\b → if b then " for: " else " against: ") model.debate.inFavor) <> DM.fromMaybe "" model.debate.topic
                    , HE.textarea'
                            [ HA.maxlength maxStatementCharacters
                            , HA.onInput' ResizeMessageInput
                            , HA.autocomplete "off"
                            , HA.class' "modal-input paper-input "
                            , HA.onInput SetDebateStatement
                            , HA.placeholder "Be as through as you'd like"
                            ]
                    ]

            , if model.debate.loading then
                    HE.div' [ HA.class' "loading" ]
              else
                    HE.input
                          [ HA.type' "button"
                          , HA.onClick StartDebate
                          , HA.disabled $ DM.isNothing model.debate.topic || DM.isNothing model.debate.inFavor || DM.isNothing model.debate.statement
                          , HA.class' "green-button"
                          , HA.value "Start debate"
                          ]
            ]

mine ∷ ExperimentsModel → Html ExperimentsMessage
mine model = HE.div [] $ map entry model.debate.mine
      where
      entry debate = HE.div [ HA.class' "paper-thrown-entry" ]
            [ HE.div [ HA.class' "paper-thrown-message" ] [ HE.text debate.topic ]
            , HE.div []
                    [ HE.text $
                            if not debate.ongoing then "Ended"
                            else if DM.isJust debate.pro && DM.isJust debate.con then "Ongoing"
                            else "Waiting"
                    ]
            ]

join model = HE.div [] [ HE.text "Currently unavailable" ]

public model = HE.div [] [ HE.text "Currently unavailable" ]
