module Shared.Im.View.SuggestionProfile (suggestionProfile, signUpCall, badges, profileContextMenu, individualSuggestion, arrow) where

import Debug
import Prelude
import Shared.Availability
import Shared.Experiments.Types
import Shared.Im.Types
import Shared.User

import Client.Common.Privilege as CCP
import Data.Array ((:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Time.Duration (Days(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Safe.Coerce as SC
import Shared.Avatar as SA
import Shared.Badge (Badge)
import Shared.Badge as SB
import Shared.DateTime (DateTimeWrapper)
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SIC
import Shared.Im.Svg (backArrow, nextArrow, closeX)
import Shared.Im.Svg as SIA
import Shared.Im.View.ChatInput as SIVC
import Shared.Im.View.Retry as SIVR
import Shared.Intl as SI
import Shared.Markdown as SM
import Shared.Privilege (Privilege(..))
import Shared.Privilege as SP
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SR
import Shared.User as SUR
import Web.HTML.HTMLElement (hidden)

-- | Displays either the current chat or a list of chat suggestions
suggestionProfile ∷ ImModel → Html ImMessage
suggestionProfile model =
      if (model.user.profileVisibility > NoTemporaryUsers || not (SP.hasPrivilege StartChats model.user)) && notChatting then
            suggestionWarning
      else if DA.null model.suggestions && notChatting then
            emptySuggestions
      else
            case SIC.maybeFindContact model.chatting model.contacts of
                  Just chatting →
                        if chatting.user.availability == Unavailable then
                              unavailable chatting.user.name
                        else if model.fullContactProfileVisible then
                              fullProfile chatting.user model
                        else
                              compactProfile chatting model
                  Nothing → suggestionCards model
      where
      notChatting = DM.isNothing model.chatting

      emptySuggestions = HE.div (HA.class' { "suggestion empty retry": true, hidden: DM.isJust model.chatting })
            ( if model.suggestionsFrom == OnlineOnly then
                    onlineOnlyFilter model : (SIVR.retryForm "No users currently online :(" $ SpecialRequest NextSuggestion)
              else
                    SIVR.retryForm "Could not find suggestions" $ SpecialRequest NextSuggestion
            )

      suggestionWarning = HE.div (HA.class' { "suggestion": true, hidden: DM.isJust model.chatting }) $ welcome model

-- | Contact was deleted, made private or they blocked the logged user
unavailable ∷ String → Html ImMessage
unavailable name =
      HE.div [ HA.class' "profile-contact" ]
            [ HE.div (HA.class' "profile-contact-top")
                    [ HE.div (HA.class' "profile-unavailable-header")
                            [ SIA.arrow [ HA.class' "svg-back-card", HA.onClick $ ToggleInitialScreen true ]
                            , HE.h1 (HA.class' "contact-name") name
                            , HE.span (HA.class' "unavailable-message") " is no longer available"
                            ]
                    ]
            ]

-- | Compact profile view shown by default
compactProfile ∷ Contact → ImModel → Html ImMessage
compactProfile contact model =
      HE.div (HA.class' { "profile-contact": true, highlighted: model.toggleModal == Tutorial Chatting })
            [ HE.div (HA.class' "profile-contact-top")
                    [ SIA.arrow [ HA.class' "svg-back-card", HA.onClick $ ToggleInitialScreen true ]
                    , HE.img $ [ SA.async, SA.decoding "lazy", HA.class' "avatar-profile", HA.src $ SA.fromAvatar contact.user.avatar ] <> showProfileAction
                    , HE.div (HA.class' "profile-contact-header" : showProfileAction)
                            [ HE.div (HA.class' "contact-name-badge") $ HE.h1 (HA.class' "contact-name") contact.user.name : badges contact.user.badges
                            , typingNotice
                            , availableStatus
                            ]
                    , HE.div [ HA.class' "profile-contact-deets" ]
                            <<< HE.div [ HA.class' "outer-user-menu" ]
                            <<< SIA.contextMenu
                            $ show CompactProfileContextMenu
                    , HE.div [ HA.class' { "user-menu": true, visible: model.toggleContextMenu == ShowCompactProfileContextMenu } ] $ profileContextMenu contact.user.id true
                    ]
            , HE.div (HA.class' "show-profile-icon-div" : showProfileAction) profileIcon

            ]
      where
      showProfileAction = [ HA.title "Click to see full profile", HA.onClick ToggleContactProfile ]

      typingNotice = HE.div (HA.class' { "typing": true, hidden: not contact.typing || not model.user.typingStatus || not contact.user.typingStatus }) "Typing..."

      availableStatus =
            HE.div
                  [ HA.class' { hidden: contact.typing && model.user.typingStatus && contact.user.typingStatus || not model.user.onlineStatus || not contact.user.onlineStatus } ]
                  $ show contact.user.availability

      profileIcon = HE.svg [ HA.class' "show-profile-icon", HA.viewBox "0 0 16 16" ]
            [ HE.rect' [ HA.x "0.01", HA.y "2", HA.width "16", HA.height "2" ]
            , HE.polygon' [ HA.class' "strokeless", HA.points "8.01 16 16.01 6 0.01 6 8.01 16" ]
            ]

-- | Full screen profile view
fullProfile ∷ User → ImModel → Html ImMessage
fullProfile user model = HE.div [ HA.class' "contact-full-profile" ] $ profileMenu : profile
      where
      profileMenu = HE.div (HA.class' "profile-top-menu")
            [ SIA.arrow [ HA.class' { "svg-back-profile": true, highlighted: model.toggleModal == Tutorial Chatting }, HA.onClick ToggleContactProfile ]
            , HE.div [ HA.class' "outer-user-menu" ]
                    $ SIA.contextMenu
                    $ show FullProfileContextMenu
            , HE.div [ HA.class' { "user-menu": true, visible: model.toggleContextMenu == ShowFullProfileContextMenu } ] $ profileContextMenu user.id true
            ]

      profile =
            [ HE.div (HA.class' "avatar-info")
                    [ HE.div [ HA.class' "big-avatar-info" ]
                            [ HE.img [ HA.src $ SA.fromAvatar user.avatar, HA.class' "big-suggestion-avatar" ]
                            , HE.div (HA.class' "big-suggestion-info")
                                    ( HE.strong (HA.class' "big-card-name") user.name
                                            : badges user.badges <> [ HE.div (HA.class' "duller") $ onlineStatus model.user user ]
                                    )
                            , HE.div (HA.class' "big-suggestion-info auto-left")
                                    ( [ HE.div_ $
                                              if user.temporary then
                                                    temporary
                                              else
                                                    [ HE.strong (HA.class' "mini-suggestion-karma") $ SI.thousands user.karma
                                                    , HE.span (HA.class' "duller") $ " karma • #" <> show user.karmaPosition
                                                    ]
                                      ]
                                            <> genderAge user
                                            <> from user
                                            <> speaks user
                                    )
                            ]
                    ]
            , HE.div (HA.class' "full-card-headline-tags")
                    ( [ HE.div (HA.class' "card-headline") user.headline
                      , HE.hr' (HA.class' "tag-ruler")
                      ] <> map (HE.span (HA.class' "tag")) user.tags <> [ HE.hr' (HA.class' "tag-ruler") ]
                    )
            , HE.div [ HA.class' "card-description", HA.title "See full profile" ]
                    [ HE.span (HA.class' "card-about-description") "About"
                    , HE.div' [ HA.innerHtml $ SM.parse user.description ]
                    ]
            ]

backingTime ∷ Html ImMessage
backingTime =
      HE.div (HA.class' "backing-suggestion-call")
            [ HE.img [ HA.class' "point-melon", HA.src $ SR.resourcePath (Left Loading) Png ]
            , HE.h2 (HA.class' "backing-suggestion-call-enjoyer") "Enjoying MeroChat??"
            , HE.text "Donate today! MeroChat depends on people like you to exist"
            , HE.br
            , HE.text "Your money will pay for server costs, marketing and development time"
            , HE.br
            , HE.div (HA.class' "donate-options")
                    [ HE.i_ "PayPal"
                    , HE.form [ HA.action "https://www.paypal.com/donate", HA.method "post", HA.target "_blank" ]
                            [ HE.input [ HA.type' "hidden", HA.name "business", HA.value "RAH62A4TZZD7L" ]
                            , HE.input [ HA.type' "hidden", HA.name "currency_code", HA.value "USD" ]
                            , HE.input [ HA.type' "image", HA.src "https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif", HA.name "submit", HA.title "PayPal - The safer, easier way to pay online!", HA.alt "Donate with PayPal button" ]
                            , HE.img [ HA.alt "", HA.src "https://www.paypal.com/en_US/i/scr/pixel.gif", HA.width "1", HA.height "1" ]
                            ]
                    , HE.i_ "Liberapay"
                    , HE.script' [ HA.src "https://liberapay.com/merochat/widgets/button.js", HA.type' "text/javascript" ]
                    , HE.a [ HA.href "https://liberapay.com/merochat/donate", HA.target "_blank" ] $ HE.img [ HA.alt "Donate using Liberapay", HA.src "https://liberapay.com/assets/widgets/donate.svg" ]

                    , HE.i_ "Patreon"
                    , HE.a [ HA.href "https://www.patreon.com/bePatron?u=41075080", HA.target "_blank", HA.class' "patreon-button" ]
                            [ HE.svg [ HA.viewBox "0 0 569 546", HA.class' "svg-patreon" ]
                                    [ HE.g_
                                            [ HE.circle' [ HA.cx "362.589996", HA.cy "204.589996", HA.r "204.589996" ]
                                            , HE.rect' [ HA.height "545.799988", HA.width "100", HA.x "0", HA.y "0" ]
                                            ]
                                    ]
                            , HE.text "Subscribe"
                            ]
                    ]
            ]

individualSuggestion ∷ Suggestion → ImModel → Html ImMessage
individualSuggestion suggestion model = HE.div (HA.class' "big-card")
      [ HE.div (HA.class' "avatar-info")
              [ HE.div [ HA.class' "big-avatar-info" ]
                      [ HE.img [ HA.src $ SA.fromAvatar suggestion.avatar, HA.class' "big-suggestion-avatar" ]
                      , HE.div (HA.class' "big-suggestion-info")
                              ( HE.strong (HA.class' "big-card-name") suggestion.name
                                      : badges suggestion.badges <> [ HE.div (HA.class' "duller") $ onlineStatus model.user suggestion ]
                              )
                      , HE.div (HA.class' "big-suggestion-info auto-left")
                              ( [ HE.div_ $
                                        if suggestion.temporary then
                                              temporary
                                        else
                                              [ HE.strong (HA.class' "mini-suggestion-karma") $ SI.thousands suggestion.karma
                                              , HE.span (HA.class' "duller") $ " karma • #" <> show suggestion.karmaPosition
                                              ]
                                ]
                                      <> genderAge suggestion
                                      <> from suggestion
                                      <> speaks suggestion

                              )
                      , HE.div [ HA.class' "close-cards", HA.title "Close suggestion", HA.onClick <<< SpecialRequest <<< ToggleModal $ HideUserMenuModal ]
                              [ closeX
                              ]
                      ]
              ]
      , HE.div_
              ( [ HE.div (HA.class' "card-headline") suggestion.headline
                , HE.hr' (HA.class' "tag-ruler")
                ] <> map (HE.span (HA.class' "tag")) suggestion.tags <> [ HE.hr' (HA.class' "tag-ruler") ]
              )
      , arrow backArrow model.freeToFetchSuggestions $ SpecialRequest PreviousSuggestion
      , HE.div [ HA.class' "card-description" ]
              [ HE.span (HA.class' "card-about-description") "About"
              , HE.div' [ HA.innerHtml $ SM.parse suggestion.description ]
              ]

      , HE.div (HA.class' "see-profile-chat suggestion-input")
              [ SIVC.chatBarInput (Left suggestion.id) ChatInputBigSuggestion model
              ]

      , arrow nextArrow model.freeToFetchSuggestions $ SpecialRequest NextSuggestion
      ]

-- | Suggestions are shown as a card list
suggestionCards ∷ ImModel → Html ImMessage
suggestionCards model =
      HE.div (HA.class' "suggestion-cards")
            [ if model.user.temporary then welcomeTemporary model.user else welcome model
            , HE.div [ HA.class' "cards" ] $ map card model.suggestions <> moreCards
            ]
      where
      moreCards =
            [ HE.div (HA.class' "card card-load-more" : if model.freeToFetchSuggestions then [ HA.onClick FetchMoreSuggestions ] else [])
                    [ HE.i_ "Load more suggestions"
                    , nextArrow
                    ]
            ]
      card suggestion =
            HE.div (HA.class' "card")
                  [ HE.div (HA.class' "avatar-info")
                          [ HE.div [ HA.class' "mini-avatar-info", HA.onClick <<< SpecialRequest <<< ToggleModal $ ShowSuggestionCard suggestion.id, HA.title "See full profile" ]
                                  [ HE.img [ HA.src $ SA.fromAvatar suggestion.avatar, HA.class' "suggestion-avatar" ]
                                  , HE.div (HA.class' "mini-suggestion-info")
                                          ( [ HE.div_
                                                    [ HE.strong (HA.class' "mini-suggestion-karma") $ SI.thousands suggestion.karma
                                                    , HE.span (HA.class' "duller") $ " karma • #" <> show suggestion.karmaPosition
                                                    ]
                                            ] <> genderAge suggestion
                                                  <> from suggestion
                                                  <> onlineStatus model.user suggestion
                                          )
                                  ]
                          ]
                  , HE.div (HA.class' "mini-name-options")
                          [ HE.strong (HA.class' "card-name") suggestion.name
                          ]
                  , HE.div_
                          ( [ HE.div (HA.class' "card-headline") suggestion.headline
                            , HE.hr' (HA.class' "tag-ruler")
                            ] <> map (HE.span (HA.class' "tag")) suggestion.tags <> [ HE.hr' (HA.class' "tag-ruler") ]
                          )
                  , HE.div [ HA.class' "card-description", HA.title "See full profile", HA.onClick <<< SpecialRequest <<< ToggleModal $ ShowSuggestionCard suggestion.id ]
                          [ HE.span (HA.class' "card-about-description") "About"
                          , HE.div' [ HA.innerHtml $ SM.parse suggestion.description ]
                          ]
                  , case model.showSuggestionChatInput of
                          Just id | suggestion.id == id →
                                HE.div [ HA.class' "see-profile-chat" ]
                                      [ HE.div (HA.class' "suggestion-input")
                                              [ SIVC.chatBarInput (Left id) ChatInputSuggestion model
                                              ]
                                      ]
                          _ → HE.div (HA.class' "see-profile-chat")
                                [ HE.input [ HA.class' "see-profile-button see-profile", HA.type' "button", HA.value "See full profile", HA.onClick <<< SpecialRequest <<< ToggleModal $ ShowSuggestionCard suggestion.id ]
                                , HE.input [ HA.class' "see-profile-button see-chat", HA.type' "button", HA.value "Chat", HA.onClick $ ToggleSuggestionChatInput suggestion.id ]
                                ]
                  ]

arrow ∷ Html ImMessage → Boolean → ImMessage → Html ImMessage
arrow svg freeTo message = HE.div (HA.class' "suggestion-arrow" : if freeTo then [ HA.onClick message ] else []) svg

genderAge ∷ Suggestion → Array (Html ImMessage)
genderAge suggestion =
      case DM.maybe [] (DA.singleton <<< HE.span_) suggestion.gender <> DM.maybe [] (DA.singleton <<< HE.span_ <<< show) suggestion.age of
            [ g, a ] → [ HE.div_ [ g, separator, a ] ]
            ga → ga

separator ∷ Html ImMessage
separator = HE.span (HA.class' "duller") " • "

onlineStatus ∷ User → Suggestion → Array (Html ImMessage)
onlineStatus user suggestion
      | not user.onlineStatus || not suggestion.onlineStatus = []
      | otherwise = [ HE.span_ $ show suggestion.availability ]

temporary =
      [ HE.span [ HA.class' "quick-sign-up" ] "Quick-sign up user"
      -- from https://thenounproject.com/icon/question-646495/
      , HE.svg [ HA.class' "svg-explain-temporary-user", HA.viewBox "0 0 752 752" ]
              [ HE.path' [ HA.d "m376 162.89c-117.53 0-213.11 95.582-213.11 213.11 0 117.53 95.582 213.11 213.11 213.11 117.53 0 213.11-95.582 213.11-213.11 0-117.53-95.582-213.11-213.11-213.11zm0 28.414c102.18 0 184.7 82.523 184.7 184.7 0 102.18-82.523 184.7-184.7 184.7-102.17 0-184.7-82.523-184.7-184.7 0-102.17 82.523-184.7 184.7-184.7zm0 66.301c-39.062 0-71.035 31.973-71.035 71.039-0.054688 3.8008 1.418 7.4688 4.0898 10.176 2.668 2.707 6.3125 4.2344 10.117 4.2344s7.4492-1.5273 10.117-4.2344c2.6719-2.707 4.1445-6.375 4.0898-10.176 0-23.711 18.914-42.625 42.621-42.625 23.711 0 42.625 18.914 42.625 42.625 0 14.742-5.9453 24.809-15.688 35.074-9.7461 10.266-23.262 19.555-35.816 29.598-3.3711 2.6992-5.3281 6.7812-5.3281 11.102v18.941c-0.054688 3.8047 1.4219 7.4688 4.0898 10.176 2.6719 2.7109 6.3164 4.2344 10.117 4.2344 3.8047 0 7.4492-1.5234 10.121-4.2344 2.668-2.707 4.1406-6.3711 4.0859-10.176v-11.988c10.352-7.9023 22.508-16.594 33.449-28.117 12.75-13.438 23.383-31.559 23.383-54.609 0-39.066-31.973-71.039-71.039-71.039zm0 198.91c-10.461 0-18.941 8.4805-18.941 18.941s8.4805 18.945 18.941 18.945c10.465 0 18.945-8.4844 18.945-18.945s-8.4805-18.941-18.945-18.941z" ]
              ]
      , HE.div (HA.class' "explain-temporary-user")
              [ HE.p_ "Quick-sign up means users that just got started on MeroChat and have yet to finish creating their account"
              , HE.p_
                      [ HE.text "You can opt to not be seen (or messaged by) quick-sign up users on the "
                      , HE.a (HA.onClick <<< SpecialRequest $ ToggleModal ShowSettings) " settings"
                      , HE.text " page"
                      ]
              ]
      ]

from ∷ Suggestion → Array (Html ImMessage)
from suggestion = DM.maybe [] (\c → [ HE.div_ [ HE.span (HA.class' "duller") "from ", HE.span_ c ] ]) suggestion.country

speaks ∷ Suggestion → Array (Html ImMessage)
speaks suggestion
      | DA.null suggestion.languages = []
      | otherwise = [ HE.div_ $ HE.span (HA.class' "duller") "speaks " : (DA.intersperse separator $ map HE.span_ suggestion.languages) ]

badges ∷ ∀ message. Array Badge → Array (Html message)
badges source = map (it <<< SB.badgeFor) source
      where
      it bf = HE.div [ HA.class' "badge", HA.title bf.description ] [ HE.img $ [ HA.width "18px", HA.height "18px", HA.class' "badge-img", HA.src $ SR.resourcePath (Left SR.Favicon) Ico ], HE.span [ HA.class' "badge-text" ] bf.text ]

profileContextMenu ∷ Int → Boolean → Array (Html ImMessage)
profileContextMenu id delete =
      [ HE.div [ HA.class' { "user-menu-item menu-item-heading": true, hidden: not delete }, HA.onClick <<< SpecialRequest <<< ToggleModal $ ConfirmDeleteChat id ] "Delete chat"
      , HE.div [ HA.class' "user-menu-item menu-item-heading", HA.onClick <<< SpecialRequest <<< ToggleModal $ ConfirmBlockUser id ] "Block"
      , HE.div [ HA.class' "user-menu-item menu-item-heading", HA.onClick <<< SpecialRequest <<< ToggleModal $ ShowReport id ] "Report"
      ]

welcomeTemporary ∷ User → Html ImMessage
welcomeTemporary { name, joined } =
      HE.div (HA.class' "card-top-header")
            [ HE.div (HA.class' "welcome") $ "Welcome, " <> name
            , signUpCall joined
            ]

signUpCall ∷ DateTimeWrapper → Html ImMessage
signUpCall joined = HE.div (HA.class' "sign-up-call")
      [ HE.text "Enjoying MeroChat?"
      , HE.a [ HA.class' "warning-temporary bold", HA.onClick <<< SpecialRequest $ ToggleModal ShowProfile ] $ " Create an account  " <> remaining
      , HE.text " to keep your chats"
      ]
      where
      remaining = case DI.floor <<< SC.coerce $ SUR.temporaryUserExpiration joined of
            0 → " today"
            1 → " until tomorrow"
            n → " in " <> show n <> " days"

welcome ∷ ImModel → Html ImMessage
welcome model = HE.div (HA.class' "card-top-welcome-filter")
      [ HE.div (HA.class' "card-top-header")
              [ HE.div (HA.class' "welcome") $ "Welcome, " <> model.user.name <> "!"
              , HE.div (HA.class' "welcome-new") $
                      if not SP.hasPrivilege StartChats model.user then
                            [ HE.span (HA.class' "no-self-start") $ CCP.notEnoughKarma "start chats" (SpecialRequest <<< ToggleModal $ ShowKarmaPrivileges)
                            ]
                      else
                            case model.user.profileVisibility of
                                  Nobody → warn "hidden"
                                  Contacts → warn "contacts only"
                                  _ →
                                        [ HE.text "Here are your newest chat suggestions"

                                        ]
              ]
      , HE.div (HA.class' "back-filter")
              [ SIA.arrow [ HA.class' { "svg-back-profile": true, hidden: not model.smallScreen }, HA.onClick $ ToggleInitialScreen true ]
              , onlineOnlyFilter model
              ]
      ]

      where
      warn level =
            [ HE.text $ "Your profile is set to " <> level <> ". Change your "
            , HE.a (HA.onClick <<< SpecialRequest $ ToggleModal ShowSettings) " settings "
            , HE.text "to see new chat suggestions"
            ]

onlineOnlyFilter ∷ ImModel → Html ImMessage
onlineOnlyFilter model =
      HE.div [ HA.class' "online-only-filter" ]
            [ HE.input [ HA.type' "checkbox", HA.checked (model.suggestionsFrom == OnlineOnly), HA.id "online-only", HA.onClick ToggleSuggestionsFromOnline ]
            , HE.label [ HA.for "online-only", HA.class' "online-only-label" ] "Show only users online"
            ]

