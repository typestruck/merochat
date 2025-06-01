module Server.Im.Template where

import Prelude
import Shared.Im.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Now as EN
import Environment (production)
import Flame (QuerySelector(..))
import Flame as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Server.Im.Types (Payload)
import Server.Template (defaultParameters)
import Server.Template as ST
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Element as SE
import Shared.Im.Unread as SIU
import Shared.Im.View as SIV
import Shared.Resource (Bundle(..), ResourceType(..), updateHash)
import Shared.Resource as SP

template ∷ Payload → Effect String
template payload = do
      lt ← EN.nowDateTime
      F.preMount (QuerySelector $ show SE.Im)
            { view: \model → ST.templateWith $ defaultParameters
                    { title = SIU.title unreadChats
                    , favicon = SIU.favicon unreadChats
                    , header =
                            [ if production then HE.script' [ HA.type' "text/javascript", HA.innerHtml "666 theme-switcher.js 666" ] --used to inline theme switcher
                              else HE.script' [ HA.type' "text/javascript", HA.src $ "/file/default/theme-switcher.js" ]
                            ]
                    , content = [ SIV.view false model ]
                    , javascript = javascript
                    , css = css
                    }
            , init:
                    { chatting: Nothing
                    , freeToFetchSuggestions: true
                    , temporaryId: 0
                    , typingIds: []
                    , modalsLoaded: []
                    , suggesting:
                            if DA.null payload.suggestions then Nothing
                            else map _.id $ DA.head payload.suggestions
                    , freeToFetchChatHistory: true
                    , suggestionsPage: 1
                    , errorMessage: ""
                    , suggestionsFrom: ThisWeek
                    , showSuggestionChatInput: Nothing
                    , lastTyping: DateTimeWrapper lt
                    , smallScreen: false
                    , showMiniChatInput: false
                    , initialScreen: true
                    , temporaryEmail: Nothing
                    , showCollapsedMiniSuggestions: false
                    , temporaryPassword: Nothing
                    , enableNotificationsVisible: false
                    , messageEnter: true
                    , editing: Nothing
                    , imageCaption: Nothing
                    , selectedImage: Nothing
                    , bugging: Nothing
                    , fullContactProfileVisible: false
                    , freeToFetchContactList: true
                    , erroredFields: []
                    , fortune: Nothing
                    , toggleContextMenu: HideContextMenu
                    , toggleModal: HideUserMenuModal
                    -- , toggleModal: if payload.user.completedTutorial then HideUserMenuModal else Tutorial Welcome
                    , toggleChatModal: HideChatModal
                    , blockedUsers: []
                    , reportReason: Nothing
                    , reportComment: Nothing
                    , imUpdated: false
                    , isWebSocketConnected: false
                    , failedRequests: []
                    , hasTriedToConnectYet: false
                    , hash: updateHash
                    , contacts: payload.contacts
                    , suggestions: payload.suggestions
                    , user: payload.user
                    }
            }
      where
      unreadChats = SIU.countUnreadChats payload.user.id payload.contacts

      javascript =
            [ HE.script' [ HA.type' "text/javascript", HA.src $ SP.bundlePath Emoji Js ]
            , HE.script' [ HA.type' "text/javascript", HA.src $ SP.bundlePath Im Js ]
            ]
      css =
            [ HE.link [ HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.bundlePath Im Css ]
            , HE.style (HA.type' "text/css")
                    """
                    svg {
                        width:20px;
                        height:20px;
                    }
                    """
            ]
