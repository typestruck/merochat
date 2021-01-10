module Server.IM.Template where

import Prelude
import Shared.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Server.Template (defaultParameters)
import Server.Template as ST
import Shared.IM.Unread as SIU
import Shared.IM.View as SIV
import Shared.Path as SP

template :: {
      contacts :: Array Contact,
      suggestions :: Array Suggestion,
      user :: IMUser
} -> Effect String
template {contacts, suggestions, user} = do
      let   unreadChats = SIU.countUnreadChats user.id contacts
            suggestionsCount = DA.length suggestions
      F.preMount (QuerySelector ".im") {
            view: \model -> ST.templateWith $ defaultParameters {
                  title = SIU.title unreadChats,
                  favicon = SIU.favicon unreadChats,
                  content = [SIV.view false model],
                  javascript = javascript,
                  css = css
            },
            init: {
                  chatting: Nothing,
                  freeToFetchSuggestions: true,
                  temporaryID: 0,
                  suggesting: if suggestionsCount == 0 then Nothing else if suggestionsCount == 1 then Just 0 else Just 1,
                  freeToFetchChatHistory:true,
                  suggestionsPage: 0,
                  errorMessage: "",
                  smallScreen: false,
                  initialScreen: true,
                  enableNotificationsVisible: false,
                  messageEnter: true,
                  imageCaption: Nothing,
                  link: Nothing,
                  linkText: Nothing,
                  selectedImage: Nothing,
                  fullContactProfileVisible: false,
                  freeToFetchContactList: true,
                  shouldSendMessage: false,
                  erroredFields: [],
                  fortune: Nothing,
                  toggleContextMenu: HideContextMenu,
                  toggleModal: HideUserMenuModal,
                  message: Nothing,
                  toggleChatModal: HideChatModal,
                  blockedUsers: [],
                  isWebSocketConnected: false,
                  failedRequests: [],
                  hasTriedToConnectYet: false,
                  contacts,
                  suggestions,
                  user
            }
      }
      where javascript = [
                  HE.script' [HA.type' "text/javascript", HA.src $ SP.pathery JS "emoji.579d0f135d969603bd63"],
                  HE.script' [HA.type' "text/javascript", HA.src $ SP.pathery JS "im.a03a108e4617efd06a35"]
            ]
            css = [
                  HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.pathery CSS "im.8afea29da29563838ee1" ]
            ]
