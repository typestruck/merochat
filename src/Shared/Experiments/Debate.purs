module Shared.Experiments.Debate where

import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Dropdown as SDRO
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
            , SDRO.dropdown SelectDropdown "Topic for debate" model.debate.topic (DM.fromMaybe "Select a topic" model.debate.topic) SetDebateTopic $
                    map (\topic → { value: topic, name: topic })
                          [ "Capitalism should be replaced by a different economic system"
                          , "Alcohol should be illegal"
                          , "All drugs should be decriminalized"
                          , "Summer is the best season of the year"
                          , "God exists"
                          , "Eating meat is unethical"
                          , "Morality is objective"
                          , "Pineapple belongs on pizza"
                          , "Taylor Swift is a bigger pop star than Michael Jackson"
                          , "Science can solve all of our problems"
                          , "Billionaires should not exist"
                          , "Cats are better than dogs"
                          , "Memes are a form of art"
                          , "AI will destroy the world"
                          , "Short form content is bad for mental health"
                          , "Climate change is the most urgent scientific issue"
                          , "Cars dependency is bad for society"
                          , "All countries should have open borders"
                          , "We are living in a simulation"
                          , "Aliens exist"
                          , "Living forever would be a good thing"
                          ]
            , HE.label [] [ HE.text "Is your position pro or against?" ]
            , SDRO.dropdown SelectDropdown "Position" (show <$> model.debate.inFavor) (DM.maybe "Select your position" (\pro → if pro then "Pro" else "Against") model.debate.inFavor) SetInFavor
                    [ { value: "true", name: "Pro" }
                    , { value: "false", name: "Against" }
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
                    HE.button [ HA.onClick StartDebate, HA.disabled $ DM.isNothing model.debate.topic || DM.isNothing model.debate.inFavor || DM.isNothing model.debate.statement, HA.class' "green-button" ] [ HE.text $ "Start debate" ]
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
