module Server.IM.Template where

import Prelude
import Shared.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Server.Template (defaultParameters)
import Server.Template as ST
import Shared.IM.Unread as SIU
import Shared.IM.View as SIV

template :: {
      contacts :: Array Contact,
      suggestions :: Array Suggestion,
      user :: IMUser
} -> Effect String
template {contacts, suggestions, user} = do
      let unreadChats = SIU.countUnreadChats user.id contacts
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
                  temporaryID: 0,
                  suggesting: if DA.null suggestions then Nothing else Just 0,
                  freeToFetchChatHistory:true,
                  suggestionsPage: 0,
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
                  userContextMenuVisible: false,
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
      --REFACTOR: js css et all must have typed routes
      where javascript = [ HE.script' [HA.type' "text/javascript", HA.src "/client/javascript/im.bundle.js"] ]
            css = [
                  HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/im.css"]
            ]
