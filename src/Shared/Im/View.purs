module Shared.Im.View where

import Prelude

import Shared.Theme as ST

import Client.Platform (isWeb)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Extra as SE
import Shared.App.Banner as SAB
import Shared.Im.Contact as SIC
import Shared.Im.Types (ImMessage(..), ImModel)
import Shared.Im.View.ChatHistory as SIVCH
import Shared.Im.View.ChatInput as SIVCI
import Shared.Im.View.ContactList as SIVCL
import Shared.Im.View.LogoMenu as SIVL
import Shared.Im.View.Modal as SIVM
import Shared.Im.View.Notification as SIVN
import Shared.Im.View.NotificationMobile as SIVNM
import Shared.Im.View.Profile as SIVP
import Shared.Im.View.UserMenu as SIVU
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SR

view ∷ Boolean → ImModel → Html ImMessage
view isClientRender model = HE.div [ HA.id "im", HA.style1 "--keyboard-height" $ show model.keyboardHeight <> "px", SE.flatten false, ST.onLayoutChange "im", HA.class' { collapsed: model.collapsedSidebar } ]
      [ HE.div [ HA.class' { "contact-box": true, "same-background": DM.isJust model.chatting, "current-mobile-screen": model.initialScreen } ]
              $ (if isClientRender && not isWeb then [] else [ SAB.banner ]) <>
              [ SIVU.userMenu model
              , HE.div [ HA.class' { "suggestion-box-error": true, flexed: model.smallScreen && not (DS.null model.errorMessage) } ] [ HE.text model.errorMessage ]
              ] <> (if isClientRender && not isWeb then [] else [ SIVN.reloadPage model.imUpdated ]) <>
              [ SIVN.prompt model.enableNotificationsVisible
              , SIVCL.contactList isClientRender model
              , SIVL.logoMenu model
              , SIVM.modals model
              ]
      , HE.div [ HA.class' { "suggestion-box": true, "current-mobile-screen": not model.initialScreen, "with-chat-background": hasChatBackground }, HA.createEventMessage "dragenter" PreventStop, HA.createEventMessage "dragover" PreventStop, HA.createEventMessage "drop" DropFile, HA.style { "backgroundImage": chatBackgroundUrl } ]
              [ HE.div [ HA.class' { "suggestion-box-error": true, "error-message-connection-lost": true, flexed: not $ DS.null model.errorMessage } ] [ HE.text model.errorMessage ]
              , SIVNM.unreadNotification model
              , SIVP.suggestionProfile model
              , SIVCH.chatHistory model
              , SIVCI.chat model
              ]
      ]
      where
      chatBackgroundSource
            | model.user.ownBackground = model.user.chatBackground
            | otherwise = (SIC.maybeFindContact model.chatting model.contacts >>= (_.chatBackground <<< _.user)) <|> model.user.chatBackground

      chatBackgroundUrl = case chatBackgroundSource of
            Just image | DM.isJust model.chatting → "url(" <> SR.resourcePath (Left $ Upload image) Ignore <> ")"
            _ → ""

      hasChatBackground = not $ DS.null chatBackgroundUrl
