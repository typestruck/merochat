module Shared.Im.View.Asks where

import Prelude

import Client.Privilege as CCP
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Symbol as TDS
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Safe.Coerce as SC
import Shared.Ask (Ask)
import Shared.Change as SCN
import Shared.DateTime as SDT
import Shared.Im.Svg as SIS
import Shared.Im.Types (ImMessage(..), PostMode(..), RetryableRequest(..), User, ImModel)
import Shared.Im.View.ChatInput as SIVC
import Shared.Markdown as SM
import Shared.Modal (Modal(..), ScreenModal(..), SpecialModal(..))
import Shared.Options.Ask (maxAskCharacters)
import Shared.Options.Post (maxPostCharacters)
import Shared.Post (Post)
import Shared.Privilege (Privilege(..))
import Shared.Privilege as SP
import Shared.Resource (maxImageSizeKB)
import Shared.User (ProfileVisibility(..))
import Type.Proxy (Proxy(..))

asked ∷ ∀ message. Ask → Html message
asked ask = HE.div [ HA.class' "ask-entry" ]
      [ HE.div [] [ HE.text ask.question ]
      , HE.div [] [ HE.text ask.answer ]
      ]

askForm ∷ ImModel → User → Html ImMessage
askForm model user =
      if not $ SP.hasPrivilege SendAsks model.user then
            CCP.notEnoughKarma "send asks" (SpecialRequest <<< ToggleModal $ Screen ShowKarmaPrivileges)
      else if user.asksVisibility == Nobody || user.asksVisibility == NoTemporaryUsers && model.user.temporary || user.asksVisibility == Contacts && not user.isContact then
            HE.div_ [ HE.text $ user.name <> " is not accepting questions right now" ]
      else if DA.elem user.id model.asks.unallowed then
            HE.div_ [ HE.text $ "You already have a pending question for " <> user.name ]
      else if DA.elem user.id model.asks.sent then
            HE.div_ [ HE.text $ "Question sent. Waiting for " <> user.name <> "'s answer" ]
      else
            form
      where
      form = HE.div [ HA.class' "asks-form" ]
            [ HE.textarea'
                    [ HA.class' "chat-input"
                    , HA.placeholder "What do you want to ask?"
                    , HA.maxlength maxAskCharacters
                    , HA.onInput' ResizeChatInput
                    , SCN.onChange (SetAsk <<< SCN.toMaybe)
                    , HA.autocomplete "off"
                    , HA.value $ DM.fromMaybe "" model.asks.question
                    ]
            , if model.asks.freeToSend then
                    HE.input [ HA.disabled $ DM.isNothing model.asks.question, HA.type' "button", HA.class' "green-button post-button build ask-button", HA.value "Ask!", HA.onClick $ SendAsk user.id ]
              else
                    HE.div' [ HA.class' "loading" ]
            ]
