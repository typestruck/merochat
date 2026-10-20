module Server.Im.Template where

import Prelude

import Effect (Effect)
import Shared.Html (Html(..))

template ∷ _ → Effect Html
template payload =
      pure $ Html "" {- do
      lt ← EN.nowDateTime
      Html <$> F.preMount (SE.toQuerySelector SE.Im)
            { view: \m → ST.templateWith defaultParameters
                    { title = SIU.title unreadChats
                    , favicon = SIU.favicon unreadChats
                    , header =
                            [ if production then SE.script' [ HA.type' "text/javascript", SE.innerHtml "666 theme-switcher.js 666" ] --used to inline theme switcher
                              else SE.script' [ HA.type' "text/javascript", HA.src $ "/file/default/theme-switcher.js" ]
                            ]
                    , content = modals : [ SIV.view false m ]
                    , javascript = javascript
                    , css = css
                    }
            , model: model lt
            }
      where
      model ∷ DateTime → ImModel
      model lt =
            { chatting: Nothing
            , freeToFetchSuggestions: true
            , temporaryId: 0
            , loadingContact: Nothing
            , suggesting:
                    if DA.null payload.suggestions then Nothing
                    else map _.id $ DA.head payload.suggestions
            , freeToFetchChatHistory: true
            , react: WithEmoji
            , suggestionsPage: 1
            , errorMessage: ""
            , showSuggestionChatInput: Nothing
            , lastTyping: DateTimeWrapper lt
            , showBuildProfile: DA.length payload.user.completedFields < 3
            , smallScreen: false
            , showMiniChatInput: false
            , initialScreen: true
            , temporaryEmail: Nothing
            , changelogs: []
            , showChangelogs: false
            , showCollapsedMiniSuggestions: false
            , collapsedSidebar: false
            , showSuggestionsPostForm: payload.user.totalPosts == 0
            , temporaryPassword: Nothing
            , enableNotificationsVisible: false
            , messageEnter: true
            , editing: Nothing
            , highlighted: Nothing
            , showLargeAvatar: false
            , imageCaption: Nothing
            , selectedImage: Nothing
            , webSocketMessages: []
            , fullContactProfileVisible: false
            , freeToFetchContactList: true
            , erroredFields: []
            , fortune: Nothing
            , toggleContextMenu: HideContextMenu
            , modal: if payload.user.completedTutorial then HideModal else Special ShowRules
            , blockedUsers: []
            , reportReason: Nothing
            , reportComment: Nothing
            , privateNote: Nothing
            , imUpdated: false
            , praise:
                    { freeToSave: true
                    , selected: Nothing
                    , other: Nothing
                    , freeToFetch: true
                    }
            , asks:
                    { freeToFetch: true
                    , freeToSend: true
                    , unallowed: []
                    , sent: []
                    , question: Nothing
                    }
            , posts:
                    { freeToFetch: true
                    , freeToSend: true
                    , mode: TextOnly
                    , text: Nothing
                    , link: Nothing
                    , image: Nothing
                    , caption: Nothing
                    }
            , webSocketStatus: Closed
            , failedRequests: []
            , modalsLoaded: []
            , hash: updateHash
            , contacts: payload.contacts
            , suggestions: payload.suggestions
            , user: payload.user
            }
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
            [ SE.script' [ HA.type' "text/javascript", HA.src $ SP.bundlePath Emoji Js ]
            , SE.script' [ HA.type' "text/javascript", HA.src $ SP.bundlePath Im Js ]
            ]
      css =
            [ SE.link [ HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.bundlePath Im Css ]
            , SE.style [ HA.type' "text/css" ]
                    [ HE.text
                            """
                    svg {
                        width:20px;
                        height:20px;
                    }
                    """
                    ]
            ]
      -}
