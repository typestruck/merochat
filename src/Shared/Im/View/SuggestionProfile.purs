module Shared.Im.View.SuggestionProfile (suggestionProfile, signUpCall, displayProfile, badges) where

import Debug
import Prelude
import Shared.Availability
import Shared.Experiments.Types
import Shared.Im.Types
import Shared.User

import Client.Common.Privilege as CCP
import Data.Array ((!!), (..), (:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.HashMap as HS
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element (class ToNode)
import Flame.Html.Element as HE
import Safe.Coerce as SC
import Shared.Avatar as SA
import Shared.Badge (Badge)
import Shared.Badge as SB
import Shared.DateTime (DateTimeWrapper)
import Shared.Element (ElementId(..))
import Shared.Im.Svg (backArrow, nextArrow)
import Shared.Im.Svg as SIA
import Shared.Im.View.ChatInput as SIVC
import Shared.Im.View.Retry as SIVR
import Shared.Intl as SI
import Shared.Markdown as SM
import Shared.Privilege (Privilege(..))
import Shared.Privilege as SP
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SR
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Shared.User as SUR
import Test.Client.Model (model)
import Web.HTML.Event.EventTypes (online)

-- | Displays either the current chat or a list of chat suggestions
suggestionProfile ∷ ImModel → Html ImMessage
suggestionProfile model@{ suggestions, contacts, suggesting, chatting, fullContactProfileVisible, user } =
      if (user.profileVisibility > NoTemporaryUsers || not (SP.hasPrivilege StartChats user)) && notChatting then
            suggestionWarning
      else if DA.null suggestions && notChatting then
            emptySuggestions
      else
            case chatting, suggesting of
                  i@(Just index), _ →
                        let
                              contact@{ user: { name, availability } } = contacts !@ index
                        in
                              if availability == Unavailable then
                                    unavailable name
                              else if fullContactProfileVisible then
                                    fullProfile FullContactProfile i model contact.user
                              else
                                    compactProfile model contact
                  Nothing, (Just index) → suggestionCards model index
                  _, _ → emptySuggestions
      where
      notChatting = DM.isNothing chatting

      emptySuggestions = HE.div (HA.class' { "suggestion empty retry": true, hidden: DM.isJust chatting })
            ( onlineOnlyFilter model :
                    (SIVR.retryForm "Could not find new suggestions" $ SpecialRequest NextSuggestion)
            )

      suggestionWarning = HE.div (HA.class' { "suggestion": true, hidden: DM.isJust chatting }) $ welcome model

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
compactProfile ∷ ImModel → Contact → Html ImMessage
compactProfile model contact =
      HE.div (HA.class' { "profile-contact": true, highlighted: model.toggleModal == Tutorial Chatting })
            [ HE.div (HA.class' "profile-contact-top")
                    [ SIA.arrow [ HA.class' "svg-back-card", HA.onClick $ ToggleInitialScreen true ]
                    , HE.img $ [ SA.async, SA.decoding "lazy", HA.class' avatarClasses, HA.src $ SA.avatarForRecipient model.chatting contact.user.avatar ] <> showProfileAction
                    , HE.div (HA.class' "profile-contact-header" : showProfileAction)
                            [ HE.div (HA.class' "contact-name-badge") $ HE.h1 (HA.class' "contact-name") contact.user.name : badges contact.user.badges
                            , typingNotice
                            , availableStatus
                            ]
                    , HE.div [ HA.class' "profile-contact-deets" ]
                            <<< HE.div [ HA.class' "outer-user-menu" ]
                            <<< SIA.contextMenu
                            $ show CompactProfileContextMenu
                    , HE.div [ HA.class' { "user-menu": true, visible: model.toggleContextMenu == ShowCompactProfileContextMenu } ] $ blockReport contact.user.id
                    ]
            , HE.div (HA.class' "show-profile-icon-div" : showProfileAction) profileIcon

            ]
      where
      showProfileAction = [ HA.title "Click to see full profile", HA.onClick ToggleContactProfile ]

      avatarClasses
            | DM.isNothing contact.user.avatar = "avatar-profile " <> SA.avatarColorClass model.chatting
            | otherwise = "avatar-profile"

      isTyping = (model.contacts !@ SU.fromJust model.chatting).typing

      typingNotice = HE.div (HA.class' { "duller typing": true, hidden: not isTyping || not model.user.typingStatus || not contact.user.typingStatus }) "Typing..."

      availableStatus =
            HE.div
                  [ HA.class' { hidden: isTyping && model.user.typingStatus && contact.user.typingStatus || not model.user.onlineStatus || not contact.user.onlineStatus, duller: contact.user.availability /= Online } ]
                  $ show contact.user.availability

      profileIcon = HE.svg [ HA.class' "show-profile-icon", HA.viewBox "0 0 16 16" ]
            [ HE.rect' [ HA.x "0.01", HA.y "2", HA.width "16", HA.height "2" ]
            , HE.polygon' [ HA.class' "strokeless", HA.points "8.01 16 16.01 6 0.01 6 8.01 16" ]
            ]

-- | Suggestion cards/full screen profile view
fullProfile ∷ ProfilePresentation → Maybe Int → ImModel → ImUser → Html ImMessage
fullProfile presentation index model@{ toggleContextMenu, freeToFetchSuggestions, toggleModal } user@{ id } =
      case presentation of
            FullContactProfile → HE.div [ HA.class' "suggestion old" ] $ fullProfileMenu : profile
            CenterCard → HE.div [ HA.class' "suggestion-center" ] -- only center card suggestion can be chatted to
                  [ HE.div [ HA.class' "suggestion new" ] $ loading : currentSuggestionMenu : profile
                  , HE.div [ HA.class' "suggestion-input" ] $ SIVC.chatBarInput ChatInputSuggestion model
                  ]
            card → HE.div [ HA.class' "suggestion new", HA.onClick <<< SpecialRequest $ if card == PreviousCard then PreviousSuggestion else NextSuggestion ] profile
      where
      fullProfileMenu = HE.div (HA.class' "profile-top-menu")
            [ SIA.arrow [ HA.class' { "svg-back-profile": true, highlighted: toggleModal == Tutorial Chatting }, HA.onClick ToggleContactProfile ]
            , HE.div [ HA.class' "outer-user-menu" ]
                    $ SIA.contextMenu
                    $ show FullProfileContextMenu
            , HE.div [ HA.class' { "user-menu": true, visible: toggleContextMenu == ShowFullProfileContextMenu } ] $ blockReport id
            ]

      profile =
            displayProfile index
                  model.user
                  user
                  (Just <<< SpecialRequest $ ToggleModal ShowSettings)
                  <>
                        [ arrow $ SpecialRequest PreviousSuggestion
                        , arrow $ SpecialRequest NextSuggestion
                        ]

      arrow message = HE.div (HA.class' "suggestion-arrow" : clickMessage)
            [ case message of
                    SpecialRequest PreviousSuggestion → backArrow
                    _ → nextArrow
            ]
            where
            clickMessage
                  | freeToFetchSuggestions = [ HA.onClick message ]
                  | otherwise = []

      currentSuggestionMenu = HE.div [ HA.class' "profile-context outer-user-menu" ]
            [ SIA.arrow [ HA.class' "svg-back-card", HA.onClick $ ToggleInitialScreen true ]
            , SIA.contextMenu $ show SuggestionContextMenu
            , HE.div [ HA.class' { "user-menu": true, visible: toggleContextMenu == ShowSuggestionContextMenu } ] <<< blockReport $ id
            ]

      loading = HE.div' $ HA.class' { loading: true, hidden: freeToFetchSuggestions }

displayProfile ∷ ∀ message. Maybe Int → ImUser → ImUser → Maybe message → Array (Html message)
displayProfile index loggedUser profileUser temporaryUserMessage =
      [ SA.avatar [ HA.class' avatarClasses, HA.src $ SA.avatarForRecipient index profileUser.avatar ]
      , HE.h1 (HA.class' "profile-name") profileUser.name
      , HE.div (HA.class' "headline") profileUser.headline
      , HE.div [ HA.class' { "online-status": true, hidden: not loggedUser.onlineStatus || not profileUser.onlineStatus, duller: profileUser.availability /= Online } ] $ show profileUser.availability
      , HE.div (HA.class' "profile-karma")
              $
                    case temporaryUserMessage of
                          Just msg | profileUser.temporary →
                                [ HE.span [ HA.class' "quick-sign-up" ] "Quick-sign up user"
                                -- from https://thenounproject.com/icon/question-646495/
                                , HE.svg [ HA.class' "svg-explain-temporary-user", HA.viewBox "0 0 752 752" ]
                                        [ HE.path' [ HA.d "m376 162.89c-117.53 0-213.11 95.582-213.11 213.11 0 117.53 95.582 213.11 213.11 213.11 117.53 0 213.11-95.582 213.11-213.11 0-117.53-95.582-213.11-213.11-213.11zm0 28.414c102.18 0 184.7 82.523 184.7 184.7 0 102.18-82.523 184.7-184.7 184.7-102.17 0-184.7-82.523-184.7-184.7 0-102.17 82.523-184.7 184.7-184.7zm0 66.301c-39.062 0-71.035 31.973-71.035 71.039-0.054688 3.8008 1.418 7.4688 4.0898 10.176 2.668 2.707 6.3125 4.2344 10.117 4.2344s7.4492-1.5273 10.117-4.2344c2.6719-2.707 4.1445-6.375 4.0898-10.176 0-23.711 18.914-42.625 42.621-42.625 23.711 0 42.625 18.914 42.625 42.625 0 14.742-5.9453 24.809-15.688 35.074-9.7461 10.266-23.262 19.555-35.816 29.598-3.3711 2.6992-5.3281 6.7812-5.3281 11.102v18.941c-0.054688 3.8047 1.4219 7.4688 4.0898 10.176 2.6719 2.7109 6.3164 4.2344 10.117 4.2344 3.8047 0 7.4492-1.5234 10.121-4.2344 2.668-2.707 4.1406-6.3711 4.0859-10.176v-11.988c10.352-7.9023 22.508-16.594 33.449-28.117 12.75-13.438 23.383-31.559 23.383-54.609 0-39.066-31.973-71.039-71.039-71.039zm0 198.91c-10.461 0-18.941 8.4805-18.941 18.941s8.4805 18.945 18.941 18.945c10.465 0 18.945-8.4844 18.945-18.945s-8.4805-18.941-18.945-18.941z" ]
                                        ]
                                , HE.div (HA.class' "explain-temporary-user duller")
                                        [ HE.p_ "Quick-sign up means users that just got started on MeroChat and have yet to finish creating their account"
                                        , HE.p_
                                                [ HE.text "You can opt to not be seen (or messaged by) quick-sign up users on the "
                                                , HE.a (HA.onClick msg) " settings"
                                                , HE.text " page"
                                                ]
                                        ]
                                ]
                          _ →
                                [ HE.div_ $
                                        [ HE.span [ HA.class' "span-info" ] $ SI.thousands profileUser.karma
                                        , HE.span [ HA.class' "duller" ] " karma"
                                        , HE.span_ $ " (#" <> show profileUser.karmaPosition <> ")"
                                        ] <> map spanWith (badges profileUser.badges)
                                ]
      , HE.div (HA.class' "profile-asl")
              [ HE.div_
                      [ toSpan $ map show profileUser.age
                      , duller (DM.isNothing profileUser.age || DM.isNothing profileUser.gender) ", "
                      , toSpan profileUser.gender
                      ]
              , HE.div_
                      [ duller (DM.isNothing profileUser.country) "from "
                      , toSpan profileUser.country
                      ]
              , HE.div_
                      ( [ duller (DA.null profileUser.languages) "speaks "
                        ] <> (DA.intercalate [ duller false ", " ] $ map (DA.singleton <<< spanWith) profileUser.languages)
                      )
              ]
      , HE.div (HA.class' "tags-description")
              [ HE.div (HA.class' "profile-tags") $ map (HE.span (HA.class' "tag")) profileUser.tags
              , HE.span (HA.class' "duller profile-description-about") "About"
              ]
      , HE.div (HA.class' "about-description")
              [ HE.div' [ HA.class' "description-message", HA.innerHtml $ SM.parse profileUser.description ] ]
      ]
      where
      avatarClasses
            | DM.isNothing profileUser.avatar = "avatar-profile " <> SA.avatarColorClass index
            | otherwise = "avatar-profile"

      toSpan = DM.maybe (HE.createEmptyElement "span") spanWith

      spanWith ∷ ∀ b m. ToNode b m Html ⇒ b → Html m
      spanWith = HE.span (HA.class' "span-info")

      duller hidden t = HE.span (HA.class' { "duller": true, "hidden": hidden }) t

badges ∷ ∀ message. Array Badge → Array (Html message)
badges source = map (it <<< SB.badgeFor) source
      where
      it bf = HE.div [ HA.class' "badge", HA.title bf.description ] [ HE.img $ [ HA.width "18px", HA.height "18px", HA.class' "badge-img", HA.src $ SR.resourcePath (Left SR.Favicon) Ico ], HE.span [ HA.class' "badge-text" ] bf.text ]

blockReport ∷ Int → Array (Html ImMessage)
blockReport id =
      [ HE.div [ HA.class' "user-menu-item menu-item-heading", HA.onClick <<< SpecialRequest <<< ToggleModal $ ConfirmBlockUser id ] "Block"
      , HE.div [ HA.class' "user-menu-item menu-item-heading", HA.onClick <<< SpecialRequest <<< ToggleModal $ ShowReport id ] "Report"
      ]

-- | Suggestions are shown as a (three) card list
suggestionCards ∷ ImModel → Int → Html ImMessage
suggestionCards model@{ user, suggestions, toggleModal } index =
      HE.div (HA.class' "suggestion-cards")
            [ if user.temporary && isNotTutorial then welcomeTemporary user else welcome model
            , HE.div (HA.class' "cards") cardTrio
            ]
      where
      cardTrio =
            let
                  available = DA.catMaybes <<< map (\i → map (card i) $ suggestions !! i) $ (index - 1) .. (index + 1)
            in
                  case DA.length available of
                        1 → dummyCard (index - 1) : DA.snoc available (dummyCard (index + 1))
                        2 | index == 0 → dummyCard (index - 1) : available
                        2 | index > 0 → DA.snoc available $ dummyCard (index + 1)
                        _ → available

      card suggesting profile =
            let
                  isCenter = suggesting == index
                  isPrevious = suggesting < index
                  attrs
                        | isCenter = [ HA.class' { "card card-center": true, highlighted: toggleModal == Tutorial ChatSuggestions } ]
                        | otherwise = [ HA.class' "card card-sides faded" ]
            in
                  HE.div attrs $ fullProfile (if isCenter then CenterCard else if isPrevious then PreviousCard else NextCard) (Just suggesting) model profile

      isNotTutorial = case toggleModal of
            Tutorial _ → false
            _ → true

      dummyCard suggesting = HE.div [ HA.class' "card card-sides faded", HA.onClick <<< SpecialRequest $ if suggesting < index then PreviousSuggestion else NextSuggestion ] $ HE.div' [ HA.class' "suggestion new invisible" ]

welcomeTemporary ∷ ImUser → Html ImMessage
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
welcome model = HE.div_
      [ HE.div (HA.class' "card-top-header")
              [ HE.div (HA.class' "welcome") $ "Welcome, " <> model.user.name
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
      , onlineOnlyFilter model
      ]

      where
      warn level =
            [ HE.text $ "Your profile is set to " <> level <> ". Change your "
            , HE.a (HA.onClick <<< SpecialRequest $ ToggleModal ShowSettings) " settings "
            , HE.text "to see new chat suggestions"
            ]

onlineOnlyFilter ∷ ImModel → Html ImMessage
onlineOnlyFilter model =
      HE.div [ HA.class' "duller online-only-filter" ]
            [ HE.input [ HA.type' "checkbox", HA.checked (model.suggestionsFrom == OnlineOnly), HA.id "online-only", HA.onClick ToggleSuggestionsFromOnline ]
            , HE.label [ HA.for "online-only", HA.class' "online-only-label" ] "Show only users online"
            ]