module Shared.IM.View.SuggestionProfile (suggestionProfile, displayProfile) where

import Debug
import Prelude
import Shared.Experiments.Types
import Shared.IM.Types
import Shared.User

import Data.Array ((!!), (..), (:))
import Data.Array as DA
import Data.HashMap as HS
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.Experiments.Impersonation (impersonations)
import Shared.Experiments.Impersonation as SEI
import Shared.IM.Svg (backArrow, nextArrow)
import Shared.IM.Svg as SIA
import Shared.IM.View.ChatInput as SIVC
import Shared.IM.View.Retry as SIVR
import Shared.Markdown as SM
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU

-- | Displays either the current chat partner or a list of chat suggestions
suggestionProfile ∷ IMModel → Html IMMessage
suggestionProfile model@{ suggestions, contacts, suggesting, chatting, fullContactProfileVisible, user } =
      if user.profileVisibility /= Everyone && notChatting then
            suggestionWarning
      else if DA.null suggestions && notChatting then
            emptySuggestions
      else
            case chatting, suggesting of
                  i@(Just index), _ →
                        let
                              contact@{ user: { name, availability }, impersonating } = contacts !@ index
                        in
                              if availability == Unavailable then
                                    unavailable name
                              else if fullContactProfileVisible then
                                    fullProfile FullContactProfile i model impersonating contact.user
                              else
                                    compactProfile model contact
                  Nothing, (Just index) → suggestionCards model index
                  _, _ → emptySuggestions
      where
      notChatting = DM.isNothing chatting

      emptySuggestions = HE.div (HA.class' { "suggestion empty retry": true, hidden: DM.isJust chatting }) $ SIVR.retryForm "Could not find new suggestions" $ SpecialRequest NextSuggestion

      suggestionWarning = HE.div (HA.class' { "suggestion": true, hidden: DM.isJust chatting }) $ welcome user

-- | Contact was deleted, made private or blocked the logged user
unavailable ∷ String → Html IMMessage
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
compactProfile ∷ IMModel → Contact → Html IMMessage
compactProfile { chatting, toggleContextMenu, contacts, user: loggedUser } contact@{ impersonating, user: { id, availability, typingStatus } } =
      HE.div (HA.class' "profile-contact")
            [ HE.div (HA.class' "profile-contact-top")
                    [ SIA.arrow [ HA.class' "svg-back-card", HA.onClick $ ToggleInitialScreen true ]
                    , HE.img $ [ HA.class' $ "avatar-profile " <> SA.avatarColorClass chatting, HA.src $ SA.avatarForRecipient chatting avatar ] <> showProfileAction
                    , HE.div (HA.class' "profile-contact-header" : showProfileAction)
                            [ HE.h1 (HA.class' "contact-name") name
                            , typingNotice
                            , availableStatus
                            ]
                    , HE.div [ HA.class' "profile-contact-deets" ]
                            <<< HE.div [ HA.class' "outer-user-menu" ]
                            <<< SIA.contextMenu
                            $ show CompactProfileContextMenu
                    , HE.div [ HA.class' { "user-menu": true, visible: toggleContextMenu == ShowCompactProfileContextMenu } ] <<< blockReport $ Tuple id impersonating
                    ]
            , HE.div (HA.class' "show-profile-icon-div" : showProfileAction) profileIcon

            ]
      where
      showProfileAction = [ HA.title "Click to see full profile", HA.onClick ToggleContactProfile ]

      { name, avatar } = case impersonating of
            Just impersonationId → SU.fromJust $ HS.lookup impersonationId impersonations
            _ → contact.user

      isTyping = (contacts !@ SU.fromJust chatting).typing

      typingNotice = HE.div (HA.class' { "duller typing": true, hidden: not isTyping || not loggedUser.typingStatus || not typingStatus }) "Typing..."

      availableStatus =
            HE.div
                  [ HA.class' { hidden: isTyping || not loggedUser.onlineStatus || not contact.user.onlineStatus, duller: availability /= Online } ]
                  $ show availability

      profileIcon = HE.svg [ HA.class' "show-profile-icon", HA.viewBox "0 0 16 16" ]
            [ HE.rect' [ HA.x "0.01", HA.y "2", HA.width "16", HA.height "2" ]
            , HE.polygon' [ HA.class' "strokeless", HA.points "8.01 16 16.01 6 0.01 6 8.01 16" ]
            ]

-- | Suggestion cards/full screen profile view
fullProfile ∷ ProfilePresentation → Maybe Int → IMModel → Maybe Int → ImUser → Html IMMessage
fullProfile presentation index model@{ toggleContextMenu, freeToFetchSuggestions } impersonating user@{ id } =
      case presentation of
            FullContactProfile → HE.div [ HA.class' "suggestion old" ] $ fullProfileMenu : profile
            CenterCard → HE.div [ HA.class' "suggestion-center" ] -- only center card suggestion can be chatted to
                  [ HE.div [ HA.class' "suggestion new" ] $ loading : currentSuggestionMenu : profile
                  , HE.div [ HA.class' "suggestion-input" ] $ SIVC.chatBarInput ChatInputSuggestion model
                  ]
            card → HE.div [ HA.class' "suggestion new", HA.onClick <<< SpecialRequest $ if card == PreviousCard then PreviousSuggestion else NextSuggestion ] profile
      where
      fullProfileMenu = HE.div (HA.class' "profile-top-menu")
            [ SIA.arrow [ HA.class' "svg-back-profile", HA.onClick ToggleContactProfile ]
            , HE.div [ HA.class' "outer-user-menu" ]
                    $ SIA.contextMenu
                    $ show FullProfileContextMenu
            , HE.div [ HA.class' { "user-menu": true, visible: toggleContextMenu == ShowFullProfileContextMenu } ] <<< blockReport $ Tuple id impersonating
            ]

      profile =
            displayProfile index
                  ( case impersonating of
                          Just impersonationId →
                                SU.fromJust $ HS.lookup impersonationId impersonations
                          _ →
                                user
                  ) <>
                  [ arrow $ SpecialRequest PreviousSuggestion
                  , arrow $ SpecialRequest NextSuggestion
                  ]

      arrow message = HE.div (HA.class' ("suggestion-arrow" <> e) : clickMessage)
            [ case message of
                    SpecialRequest PreviousSuggestion → backArrow
                    _ → nextArrow
            ]
            where
            clickMessage
                  | freeToFetchSuggestions = [ HA.onClick message ]
                  | otherwise = []
            e = case message of
                  SpecialRequest PreviousSuggestion → " ppe"
                  _ → ""

      currentSuggestionMenu = HE.div [ HA.class' "profile-context outer-user-menu" ]
            [ SIA.arrow [ HA.class' "svg-back-card", HA.onClick $ ToggleInitialScreen true ]
            , SIA.contextMenu $ show SuggestionContextMenu
            , HE.div [ HA.class' { "user-menu": true, visible: toggleContextMenu == ShowSuggestionContextMenu } ] <<< blockReport $ Tuple id impersonating
            ]

      loading = HE.div' $ HA.class' { loading: true, hidden: freeToFetchSuggestions }

displayProfile ∷ ∀ message. Maybe Int → ImUser → Array (Html message)
displayProfile index { karmaPosition, name, availability, avatar, age, karma, headline, gender, country, languages, tags, description } =
      [ HE.img [ HA.class' $ "avatar-profile " <> SA.avatarColorClass index, HA.src $ SA.avatarForRecipient index avatar ]
      , HE.h1 (HA.class' "profile-name") name
      , HE.div (HA.class' "headline") headline
      , HE.div [ HA.class' { "online-status": true, duller: availability /= Online } ] $ show availability
      , HE.div (HA.class' "profile-karma")
              [ HE.div_
                      [ HE.span [ HA.class' "span-info" ] $ show karma
                      , HE.span [ HA.class' "duller" ] " karma"
                      , HE.span_ $ " (#" <> show karmaPosition <> ")"
                      ]
              ]
      , HE.div (HA.class' "profile-asl")
              [ HE.div_
                      [ toSpan $ map show age
                      , duller (DM.isNothing age || DM.isNothing gender) ", "
                      , toSpan gender
                      ]
              , HE.div_
                      [ duller (DM.isNothing country) "from "
                      , toSpan country
                      ]
              , HE.div_
                      ( [ duller (DA.null languages) "speaks "
                        ] <> (DA.intercalate [ duller false ", " ] $ map (DA.singleton <<< spanWith) languages)
                      )
              ]
      , HE.div (HA.class' "tags-description")
              [ HE.div (HA.class' "profile-tags") $ map (HE.span (HA.class' "tag")) tags
              , HE.span (HA.class' "duller profile-description-about") "About"
              , HE.div' [ HA.class' "description-message", HA.innerHtml $ SM.parse description ]
              ]
      ]
      where
      toSpan = DM.maybe (HE.createEmptyElement "span") spanWith

      spanWith = HE.span (HA.class' "span-info")

      duller hidden t = HE.span (HA.class' { "duller": true, "hidden": hidden }) t

blockReport ∷ Tuple Int (Maybe Int) → Array (Html IMMessage)
blockReport tupleId =
      [ HE.div [ HA.class' "user-menu-item menu-item-heading", HA.onClick <<< SpecialRequest <<< ToggleModal $ ConfirmBlockUser tupleId ] "Block"
      , HE.div [ HA.class' "user-menu-item menu-item-heading", HA.onClick <<< SpecialRequest <<< ToggleModal <<< ShowReport $ DT.fst tupleId ] "Report"
      ]

-- | Suggestions are shown as a (three) card list
suggestionCards ∷ IMModel → Int → Html IMMessage
suggestionCards model@{ user, suggestions, experimenting } index =
      HE.div (HA.class' "suggestion-cards")
            [ case experimenting of
                    Just (Impersonation (Just { name })) → welcomeImpersonation name
                    _ → welcome user
            , HE.div (HA.class' "cards") cardTrio
            ]
      where
      cardTrio =
            let
                  dummyCard i = card i dummySuggestion
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
                        | isCenter = [ HA.class' "card card-center" ]
                        | otherwise = [ HA.class' "card card-sides faded" ]
            in
                  HE.div attrs $ fullProfile (if isCenter then CenterCard else if isPrevious then PreviousCard else NextCard) (Just suggesting) model Nothing profile

      welcomeImpersonation name =
            let
                  { welcome, first, second } = SEI.welcomeMessage name
            in
                  HE.div (HA.class' "card-top-header imp")
                        [ HE.div (HA.class' "welcome") $ welcome
                        , HE.div (HA.class' "welcome-new") $ first <> second
                        ]

welcome ∷ ImUser → Html IMMessage
welcome { name, profileVisibility } = HE.div (HA.class' "card-top-header")
      [ HE.div (HA.class' "welcome") $ "Welcome, " <> name
      , HE.div (HA.class' "welcome-new") $ case profileVisibility of
              Nobody → warn "hidden"
              Contacts → warn "contacts only"
              _ → [ HE.text "Here are your newest chat suggestions" ]
      ]
      where
      warn level =
            [ HE.text $ "Your profile is set to " <> level <> ". Change your "
            , HE.a (HA.onClick (SpecialRequest $ ToggleModal ShowSettings)) " settings "
            , HE.text "to see new chat suggestions"
            ]

--might be better to just make a grayed out card...
-- | Fake profile so there is always three suggestion cards on display
dummySuggestion ∷ Suggestion
dummySuggestion =
      { id: 0
      , name: "Maria Navarro"
      , headline: "This is my headline, there are many like it, but this one is mine"
      , description: "Many years later, as he faced the firing squad, Colonel Aureliano Buendía was to remember that distant afternoon when his father took him to discover ice. At that time Macondo was a village of twenty adobe houses, built on the bank of a river of clear water that ran along a bed of polished stones, which were white and enormous, like prehistoric eggs. The world was so recent that many things lacked names, and in order to indicate them it was necessary to point. Every year during the month of March a family of ragged gypsies would set up their tents near the village, and with a great uproar of pipes and kettledrums they would display new inventions. First they brought the magnet."
      , avatar: Nothing
      , tags: []
      , availability: Online
      , profileVisibility: Everyone
      , readReceipts: true
      , messageTimestamps: true
      , typingStatus: true
      , onlineStatus: true
      , karma: 321
      , karmaPosition: 90
      , gender: Just $ show Female
      , country: Just "Cali"
      , languages: [ "English", "Spanish" ]
      , age: Just 18
      }
