module Server.Im.Template where

import Prelude
import Shared.Im.Types

import Data.Array ((:))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Now as EN
import Environment (production)
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
import Shared.Modal.Types (Modal(..))
import Shared.Resource (Bundle(..), ResourceType(..), updateHash)
import Shared.Resource as SP
import Web.DOM.ParentNode (QuerySelector(..))

template ∷ Payload → Effect String
template payload = do
      lt ← EN.nowDateTime
      F.preMount (QuerySelector $ "#" <> show SE.Im)
            { view: \model → ST.templateWith defaultParameters
                    { title = SIU.title unreadChats
                    , favicon = SIU.favicon unreadChats
                    , header =
                            [ if production then HE.script' [ HA.type' "text/javascript", HA.innerHtml "666 theme-switcher.js 666" ] --used to inline theme switcher
                              else HE.script' [ HA.type' "text/javascript", HA.src $ "/file/default/theme-switcher.js" ]
                            ]
                    , content = modals : [ SIV.view false model ]
                    , javascript = javascript
                    , css = css
                    }
            , model:
                    { chatting: Nothing
                    , freeToFetchSuggestions: true
                    , temporaryId: 0
                    , typingIds: []
                    , suggesting:
                            if DA.null payload.suggestions then Nothing
                            else map _.id $ DA.head payload.suggestions
                    , freeToFetchChatHistory: true
                    , suggestionsPage: 1
                    , errorMessage: ""
                    , suggestionsFrom: ThisWeek
                    , showSuggestionChatInput: Nothing
                    , lastTyping: DateTimeWrapper lt
                    , showBuildProfile: DA.length payload.user.completedFields < 3
                    , smallScreen: false
                    , showMiniChatInput: false
                    , initialScreen: true
                    , temporaryEmail: Nothing
                    , freeToFetchPosts: true
                    , changelogs: []
                    , showChangelogs: false
                    , showCollapsedMiniSuggestions: false
                    , showPostForm: if payload.user.totalPosts == 0 then SuggestionsPostForm else NoPostForm
                    , temporaryPassword: Nothing
                    , enableNotificationsVisible: false
                    , messageEnter: true
                    , editing: Nothing
                    , showLargeAvatar: false
                    , imageCaption: Nothing
                    , selectedImage: Nothing
                    , webSocketMessages: []
                    , fullContactProfileVisible: false
                    , freeToFetchContactList: true
                    , freeToPost: true
                    , erroredFields: []
                    , fortune: Nothing
                    , toggleContextMenu: HideContextMenu
                    , modal: HideModal
                    , blockedUsers: []
                    , reportReason: Nothing
                    , reportComment: Nothing
                    , imUpdated: false
                    , postContent: Nothing
                    , webSocketStatus: Closed
                    , failedRequests: []
                    , modalsLoaded: []
                    , hash: updateHash
                    , contacts: payload.contacts
                    , suggestions: payload.suggestions
                    , user: payload.user
                    }
            }
      where
      modals =
            HE.div [ HA.id "modal-root" ]
                  [ HE.div' [ HA.id $ show SE.ProfileEditionRoot ]
                  , HE.div' [ HA.id $ show SE.SettingsEditionRoot ]
                  , HE.div' [ HA.id $ show SE.KarmaPrivilegesRoot ]
                  , HE.div' [ HA.id $ show SE.ExperimentsRoot ]
                  , HE.div' [ HA.id $ show SE.BackerRoot ]
                  , HE.div' [ HA.id $ show SE.HelpRoot ]
                  , HE.div' [ HA.id $ show SE.FeedbackRoot ]
                  ]

      unreadChats = SIU.countUnreadChats payload.user.id payload.contacts
      javascript =
            [ HE.script' [ HA.type' "text/javascript", HA.src $ SP.bundlePath Emoji Js ]
            , HE.script' [ HA.type' "text/javascript", HA.src $ SP.bundlePath Im Js ]
            ]
      css =
            [ HE.link [ HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.bundlePath Im Css ]
            , HE.style [ HA.type' "text/css" ]
                    [ HE.text
                            """
                    svg {
                        width:20px;
                        height:20px;
                    }
                    """
                    ]
            ]

