module Shared.Im.View.Profile (suggestionProfile, signUpCall, badges, profileContextMenu, individualSuggestion, miniSuggestions, separator) where

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
import Data.Int as DN
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Set as DS
import Data.Time.Duration (Days(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Safe.Coerce as SC
import Shared.Avatar as SA
import Shared.Backer.Contact (backerId)
import Shared.Badge (Badge)
import Shared.Badge as SB
import Shared.DateTime (DateTimeWrapper)
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SIC
import Shared.Im.Svg (backArrow, home, nextArrow)
import Shared.Im.Svg as SIA
import Shared.Im.View.ChatInput as SIVC
import Shared.Im.View.Posts as SIVP
import Shared.Im.View.Retry as SIVR
import Shared.Intl as SI
import Shared.Markdown as SM
import Shared.Modal.Types (ConfirmationModal(..), Modal(..), ScreenModal(..), SpecialModal(..))
import Shared.Privilege (Privilege(..))
import Shared.Privilege as SP
import Shared.ProfileColumn (ProfileColumn(..))
import Shared.ProfileColumn as SPC
import Shared.Resource (ResourceType(..))
import Shared.Resource as SR
import Shared.Svg as SS
import Shared.Unsafe as SU
import Shared.User as SUR

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

      emptySuggestions = HE.div [ HA.class' { "suggestion empty retry": true, hidden: DM.isJust model.chatting } ]
            ( if model.suggestionsFrom == OnlineOnly then
                    onlineOnlyFilter model : (SIVR.retryForm "No users currently online :(" $ SpecialRequest NextSuggestion)
              else
                    SIVR.retryForm "Could not find suggestions" $ SpecialRequest NextSuggestion
            )

      suggestionWarning = HE.div [ HA.class' { "suggestion": true, hidden: DM.isJust model.chatting } ] [ welcome model ]

-- | Contact was deleted, made private or they blocked the logged user
unavailable ∷ String → Html ImMessage
unavailable name =
      HE.div [ HA.class' "profile-contact" ]
            [ HE.div [ HA.class' "profile-contact-top" ]
                    [ HE.div [ HA.class' "profile-unavailable-header" ]
                            [ SIA.arrow [ HA.class' "svg-back-card", HA.onClick $ ToggleInitialScreen true ]
                            , HE.h1 [ HA.class' "contact-name" ] [ HE.text name ]
                            , HE.span [ HA.class' "unavailable-message" ] [ HE.text " is no longer available" ]
                            ]
                    ]
            ]

-- | Compact profile view shown by default
compactProfile ∷ Contact → ImModel → Html ImMessage
compactProfile contact model =
      HE.div [ HA.class' "profile-contact" ]
            [ HE.div [ HA.class' "profile-contact-top" ]
                    [ SIA.arrow [ HA.class' "svg-back-card", HA.onClick $ ToggleInitialScreen true ]
                    , HE.img $ [ SA.async, SA.decoding "lazy", HA.class' "avatar-profile", HA.src $ SA.fromAvatar contact.user ] <> showProfileAction
                    , HE.div (HA.class' "profile-contact-header" : showProfileAction)
                            [ HE.div [ HA.class' "contact-name-badge" ] $ HE.h1 [ HA.class' "contact-name" ] ([ HE.text contact.user.name ]) : badges contact.user.badges
                            , typingNotice
                            , availableStatus
                            ]
                    , HE.div [ HA.class' "profile-contact-deets" ]
                            [ HE.div [ HA.class' "outer-user-menu" ] [ SIA.contextMenu $ show CompactProfileContextMenu ]
                            ]
                    , HE.div [ HA.class' { "user-menu": true, visible: model.toggleContextMenu == ShowCompactProfileContextMenu } ] $ profileContextMenu contact.user.id true
                    , SIA.closeX [ HA.class' "svg-close-profile", HA.onClick ResumeSuggesting ]
                    ]
            ]
      where
      showProfileAction = [ HA.title "Click to see full profile", HA.onClick ToggleContactProfile ]

      typingNotice = HE.div [ HA.class' { "typing": true, hidden: not contact.typing || not model.user.typingStatus || not contact.user.typingStatus } ] [ HE.text "Typing..." ]

      availableStatus =
            HE.div
                  [ HA.class' { hidden: contact.typing && model.user.typingStatus && contact.user.typingStatus || not model.user.onlineStatus || not contact.user.onlineStatus } ]
                  [ HE.text $ show contact.user.availability ]

-- | Full screen profile view
fullProfile ∷ User → ImModel → Html ImMessage
fullProfile user model = HE.div [ HA.class' "contact-full-profile" ] $ profileMenu : profile
      where
      profileMenu = HE.div [ HA.class' "profile-top-menu" ]
            [ SIA.arrow [ HA.class' "svg-back-profile", HA.onClick $ if model.showLargeAvatar then ToggleLargeAvatar else ToggleContactProfile ]
            , HE.div [ HA.class' { "outer-user-menu": true, hidden: model.showLargeAvatar } ]
                    [ SIA.contextMenu $ show FullProfileContextMenu
                    ]
            , HE.div [ HA.class' { "user-menu": true, visible: model.toggleContextMenu == ShowFullProfileContextMenu } ] $ profileContextMenu user.id true
            , SIA.closeX [ HA.class' "svg-close-profile", HA.onClick $ if model.showLargeAvatar then ToggleLargeAvatar else ToggleContactProfile ]
            ]

      profile =
            if model.showLargeAvatar then
                  [ HE.div [ HA.class' "avatar-info" ]
                          [ HE.div [ HA.class' "big-suggestion-info" ]
                                  [ HE.strong [ HA.class' "big-card-name big-name-avatar" ] [ HE.text user.name ]
                                  ]
                          , HE.div [ HA.class' "big-avatar-info big-avatar-center" ]
                                  [ HE.img [ HA.src $ SA.fromAvatar user, HA.title "Close avatar", HA.class' "bigger-suggestion-avatar", HA.onClick ToggleLargeAvatar ]
                                  ]
                          ]
                  ]
            else
                  [ HE.div [ HA.class' "avatar-info" ]
                          [ HE.div [ HA.class' "big-avatar-info" ]
                                  [ HE.img [ HA.src $ SA.fromAvatar user, HA.title "Open avatar", HA.class' "big-suggestion-avatar", HA.onClick ToggleLargeAvatar ]
                                  , HE.div [ HA.class' "big-suggestion-info" ]
                                          ( HE.strong [ HA.class' "big-card-name" ] [ HE.text user.name ]
                                                  : badges user.badges <> [ HE.div [ HA.class' "duller" ] $ onlineStatus model.user user ]
                                          )
                                  , HE.div [ HA.class' "big-suggestion-info auto-left" ]
                                          ( [ HE.div_ $
                                                    if user.temporary then
                                                          temporary
                                                    else
                                                          [ HE.strong [ HA.class' "mini-suggestion-karma" ] [ HE.text $ SI.thousands user.karma ]
                                                          , HE.span [ HA.class' "duller" ] [ HE.text $ " karma • #" <> show user.karmaPosition ]
                                                          ]
                                            ]
                                                  <> genderAge user
                                                  <> from user
                                                  <> speaks user
                                          )
                                  ]
                          ]
                  , HE.div [ HA.class' "full-card-headline-tags" ]
                          ( [ HE.div [ HA.class' "card-headline" ] [ HE.text user.headline ]
                            , HE.hr' [ HA.class' "tag-ruler" ]
                            ] <> map (\c → HE.span [ HA.class' "tag" ] [ HE.text c ]) user.tags <> [ HE.hr' [ HA.class' "tag-ruler" ] ]
                          )
                  , HE.div [ HA.class' "card-description", HA.title "See full profile" ]
                          [ HE.span [ HA.class' "card-about-description" ] [ HE.text "About" ]
                          , HE.div' [ HA.innerHtml $ SM.parse user.description ]
                          ]
                  ]

individualSuggestion ∷ Suggestion → ImModel → Html ImMessage
individualSuggestion suggestion model = HE.div [ HA.class' "big-card" ] $
      if model.showLargeAvatar then
            [ HE.div [ HA.class' "avatar-info" ]
                    [ HE.div [ HA.class' "big-suggestion-header" ]
                            [ HE.strong [ HA.class' "big-card-name grown" ] [ HE.text suggestion.name ]
                            , HE.div [ HA.class' "close-cards", HA.title "Close avatar", HA.onClick ToggleLargeAvatar ]
                                    [ SIA.closeX []
                                    ]
                            ]
                    , HE.div [ HA.class' "big-avatar-info big-avatar-center" ]
                            [ HE.img [ HA.title "Close avatar", HA.onClick ToggleLargeAvatar, HA.src $ SA.fromAvatar suggestion, HA.class' "bigger-suggestion-avatar" ]
                            ]
                    ]
            ]
      else
            [ HE.div [ HA.class' "back-filter" ]
                    [ SIA.arrow [ HA.class' "svg-back-profile hidden", HA.onClick <<< SpecialRequest <<< ToggleModal $ HideModal ] ]
            , HE.div [ HA.class' "suggestion-arrow-mobile" ]
                    [ arrow backArrow model.freeToFetchSuggestions [] $ SpecialRequest PreviousSuggestion
                    , arrow nextArrow model.freeToFetchSuggestions [ "next-arrow" ] $ SpecialRequest NextSuggestion
                    ]
            , HE.div [ HA.class' "avatar-info" ]
                    [ HE.div [ HA.class' "big-avatar-info" ]
                            [ HE.div [ HA.class' "full" ] [ HE.img [ HA.title "Open avatar", HA.onClick ToggleLargeAvatar, HA.src $ SA.fromAvatar suggestion, HA.class' "big-suggestion-avatar" ] ]
                            , HE.div [ HA.class' "big-suggestion-info full" ]
                                    ( HE.strong [ HA.class' "big-card-name" ] [ HE.text suggestion.name ]
                                            : badges suggestion.badges <> [ HE.div [ HA.class' "duller" ] $ onlineStatus model.user suggestion ]
                                    )
                            , HE.div [ HA.class' "big-suggestion-info auto-left order-2" ]
                                    ( [ HE.div_ $
                                              if suggestion.temporary then
                                                    temporary
                                              else
                                                    [ HE.strong [ HA.class' "mini-suggestion-karma" ] [ HE.text $ SI.thousands suggestion.karma ]
                                                    , HE.span [ HA.class' "duller" ] [ HE.text $ " karma • #" <> show suggestion.karmaPosition ]
                                                    ]
                                      ]
                                            <> [ HE.div_ $ genderAge suggestion <> countrySeparator <> from suggestion ]
                                            <> speaks suggestion
                                            <> postsCount

                                    )
                            , HE.div [ HA.class' "outer-user-menu order-3" ] [ SIA.contextMenu $ show FullProfileContextMenu ]
                            , HE.div [ HA.class' { "user-menu": true, visible: model.toggleContextMenu == ShowFullProfileContextMenu } ] $ profileContextMenu suggestion.id false
                            , HE.div [ HA.class' "close-cards", HA.title "Close suggestion", HA.onClick <<< SpecialRequest <<< ToggleModal $ HideModal ]
                                    [ SIA.closeX []
                                    ]
                            ]
                    ]

            , HE.div [ HA.class' "green-tab" ]
                    [ HE.div [ HA.onClick $ ToggleShowing suggestion.id ShowInfo, HA.class' { "regular-green-tab": true, "selected-green-tab": suggestion.showing == ShowInfo } ] [ HE.text "Info" ]
                    , HE.div [ HA.onClick $ ToggleShowing suggestion.id ShowPosts, HA.class' { "regular-green-tab": true, "selected-green-tab": suggestion.showing == ShowPosts } ] [ HE.text "Posts" ]
                    ]

            , HE.div [ HA.class' { hidden: suggestion.showing == ShowPosts } ]
                    ( [ HE.div [ HA.class' "card-headline" ] [ HE.text suggestion.headline ]
                      , HE.hr' [ HA.class' "tag-ruler" ]
                      ] <> map (\c → HE.span [ HA.class' "tag" ] [ HE.text c ]) suggestion.tags <> [ HE.hr' [ HA.class' "tag-ruler" ] ]
                    )
            , HE.div [ HA.class' { "card-description": true, hidden: suggestion.showing == ShowPosts } ]
                    [ HE.span [ HA.class' "card-about-description" ] [ HE.text "About" ]
                    , HE.div' [ HA.innerHtml $ SM.parse suggestion.description ]
                    ]

            , HE.div [ HA.class' { posts: true, hidden: suggestion.showing == ShowInfo } ]
                    [ SIVR.retry "Failed to load posts" (FetchPosts suggestion.id) model.failedRequests
                    , if model.freeToFetchPosts && DA.null suggestion.posts then
                            HE.div_ [ HE.text $ suggestion.name <> " has not posted yet" ]
                      else if model.freeToFetchPosts then
                            HE.div [ HA.class' "post-list" ] $ map (SIVP.posted suggestion.name) suggestion.posts
                      else
                            HE.div' [ HA.class' "loading" ]
                    ]

            , HE.div [ HA.class' "see-profile-chat suggestion-input" ]
                    $
                          if suggestion.isContact then
                                [ HE.input [ HA.class' "see-profile-button see-chat", HA.type' "button", HA.value "Open chat", HA.onClick $ ResumeSuggestionChat suggestion.id ] ]
                          else if suggestion.id == backerId then
                                [ HE.input [ HA.class' "see-profile-button see-chat", HA.type' "button", HA.value "See donation options", HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowBacker ] ]
                          else
                                [ SIVC.chatBarInput (Left suggestion.id) ChatInputBigSuggestion model ]

            ]
      where
      countrySeparator
            | DM.isJust suggestion.country && (DM.isJust suggestion.gender || DM.isJust suggestion.age) = [separator]
            | otherwise = []

      postsCount
            | suggestion.totalPosts == 0 = []
            | otherwise = [ HE.span_ [ HE.text $ show suggestion.totalPosts <> " posts" ] ]

-- | Suggestions are shown as a card list
suggestionCards ∷ ImModel → Html ImMessage
suggestionCards model =
      HE.div [ HA.class' "suggestion-cards" ]
            [ if model.user.temporary then welcomeTemporary model else welcome model
            , HE.div [ HA.class' "cards", HA.id $ show Cards ] allCards
            ]
      where
      allCards
            | not model.user.temporary && model.showBuildProfile = buildProfile : map card model.suggestions <> moreCards
            | otherwise = map card model.suggestions <> moreCards

      buildProfile =
            let
                  completed = DS.fromFoldable model.user.completedFields
                  missing = DS.difference (DS.fromFoldable [ Name, Headline, Description, Avatar, Birthday, Gender, Country, Languages, Tags ]) completed
                  percentage = DN.floor ((DN.toNumber $ DS.size completed) / 9.0 * 100.0)
            in
                  HE.div [ HA.class' "card build-profile" ]
                        [ HE.div [ HA.class' "avatar-info" ]
                                [ HE.div [ HA.class' "mini-avatar-info" ]
                                        [ HE.img [ HA.src $ SA.fromAvatar model.user, HA.class' "suggestion-avatar" ]
                                        , HE.div [ HA.class' "mini-suggestion-info" ]
                                                [ HE.div [ HA.class' "build-call" ] [ HE.text "Complete your profile" ]
                                                , HE.div_
                                                        [ HE.span [ HA.class' "duller" ] [ HE.text $ (show percentage) <> "% done" ]
                                                        ]
                                                ]
                                        ]
                                ]
                        , HE.div [ HA.class' "build-list" ] (map check (DS.toUnfoldable completed) <> map uncheck (DS.toUnfoldable missing))
                        , HE.div [ HA.class' "see-profile-chat" ]
                                [ HE.input [ HA.type' "button", HA.onClick HideBuildProfile, HA.class' "cancel", HA.value "Dismiss" ]
                                , HE.input [ HA.type' "button", HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowProfile, HA.class' "green-button bigger build", HA.value "Go to profile" ]
                                ]
                        ]
      check p = HE.div [ HA.class' "build-checked" ] [ SS.checked "", HE.text $ SPC.displayColumn p ]
      uncheck p = HE.div_ [ HE.text $ "➡ " <> show p ]

      moreCards =
            [ HE.div (HA.class' "card card-load-more" : if model.freeToFetchSuggestions then [ HA.onClick FetchMoreSuggestions ] else [])
                    [ HE.i_ [ HE.text "Load more suggestions" ]
                    , nextArrow
                    ]
            ]
      card suggestion =
            HE.div [ HA.class' "card" ]
                  [ HE.div [ HA.class' "avatar-info" ]
                          [ HE.div (HA.class' "mini-avatar-info" : showProfile suggestion.id)
                                  [ HE.img [ HA.src $ SA.fromAvatar suggestion, HA.class' "suggestion-avatar" ]
                                  , HE.div [ HA.class' "mini-suggestion-info" ]
                                          ( [ HE.div_
                                                    [ HE.strong [ HA.class' "mini-suggestion-karma" ] [ HE.text $ SI.thousands suggestion.karma ]
                                                    , HE.span [ HA.class' "duller" ] [ HE.text $ " karma • #" <> show suggestion.karmaPosition ]
                                                    ]
                                            ] <> genderAge suggestion
                                                  <> from suggestion
                                                  <> onlineStatus model.user suggestion
                                          )
                                  ]
                          ]
                  , HE.div [ HA.class' "card-name-online" ]
                          [ HE.strong [ HA.class' "card-name" ] [ HE.text suggestion.name ]
                          , HE.div' [ HA.class' { "online-indicator": true, hidden: suggestion.availability /= Online || not suggestion.onlineStatus || not model.user.onlineStatus } ]
                          ]
                  , HE.div [ HA.class' "rest-card" ]
                          [ HE.div [ HA.class' "card-headline" ] [ HE.text suggestion.headline ]
                          , HE.hr' [ HA.class' "tag-ruler" ]
                          , HE.div_ $ map (\c → HE.span [ HA.class' "tag" ] [ HE.text c ]) suggestion.tags <> [ HE.hr' [ HA.class' "tag-ruler" ] ]
                          , HE.div (HA.class' "card-description" : showProfile suggestion.id)
                                  [ HE.span [ HA.class' "card-about-description" ] [ HE.text "About" ]
                                  , HE.div' [ HA.innerHtml $ SM.parse suggestion.description ]
                                  ]
                          ]
                  , case model.showSuggestionChatInput of
                          Just id | suggestion.id == id →
                                HE.div [ HA.class' "see-profile-chat" ]
                                      [ HE.div [ HA.class' "suggestion-input" ]
                                              [ SIVC.chatBarInput (Left id) ChatInputSuggestion model
                                              ]
                                      ]
                          _ → HE.div [ HA.class' "see-profile-chat" ]
                                [ HE.input [ HA.class' "see-profile-button see-profile", HA.type' "button", HA.value "See full profile", HA.onClick <<< SpecialRequest <<< ToggleModal <<< Special $ ShowSuggestionCard suggestion.id ]
                                , HE.input
                                        ( [ HA.class' "see-profile-button see-chat", HA.type' "button" ] <>
                                                ( if suggestion.isContact then
                                                        [ HA.value "Open chat", HA.onClick $ ResumeSuggestionChat suggestion.id ]
                                                  else if suggestion.id == backerId then
                                                        [ HA.value "Donate", HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowBacker ]
                                                  else
                                                        [ HA.value "Chat", HA.onClick $ ToggleSuggestionChatInput suggestion.id ]
                                                )
                                        )
                                ]
                  ]
      showProfile id = [ HA.title "See full profile", HA.onClick <<< SpecialRequest <<< ToggleModal <<< Special $ ShowSuggestionCard id ]

arrow ∷ Html ImMessage → Boolean → Array String → ImMessage → Html ImMessage
arrow svg freeTo extraClasses message = HE.div (HA.class' ("suggestion-arrow" : extraClasses) : if freeTo then [ HA.onClick message ] else []) [ svg ]

genderAge ∷ Suggestion → Array (Html ImMessage)
genderAge suggestion =
      case DM.maybe [] (\c → [ HE.span_ [ HE.text c ] ]) suggestion.gender <> DM.maybe [] (\c → [ HE.span_ [ HE.text $ show c ] ]) suggestion.age of
            [ g, a ] → [ HE.div_ [ g, separator, a ] ]
            ga → ga

separator ∷ ∀ m. Html m
separator = HE.span [ HA.class' "duller" ] [ HE.text " • " ]

onlineStatus ∷ User → Suggestion → Array (Html ImMessage)
onlineStatus user suggestion
      | not user.onlineStatus || not suggestion.onlineStatus = []
      | otherwise = [ HE.span_ [ HE.text $ show suggestion.availability ] ]

temporary ∷ Array (Html ImMessage)
temporary =
      [ HE.span [ HA.class' "quick-sign-up" ] [ HE.text "Quick-sign up user" ]
      -- from https://thenounproject.com/icon/question-646495/
      , HE.svg [ HA.class' "svg-explain-temporary-user", HA.viewBox "0 0 752 752" ]
              [ HE.path' [ HA.d "m376 162.89c-117.53 0-213.11 95.582-213.11 213.11 0 117.53 95.582 213.11 213.11 213.11 117.53 0 213.11-95.582 213.11-213.11 0-117.53-95.582-213.11-213.11-213.11zm0 28.414c102.18 0 184.7 82.523 184.7 184.7 0 102.18-82.523 184.7-184.7 184.7-102.17 0-184.7-82.523-184.7-184.7 0-102.17 82.523-184.7 184.7-184.7zm0 66.301c-39.062 0-71.035 31.973-71.035 71.039-0.054688 3.8008 1.418 7.4688 4.0898 10.176 2.668 2.707 6.3125 4.2344 10.117 4.2344s7.4492-1.5273 10.117-4.2344c2.6719-2.707 4.1445-6.375 4.0898-10.176 0-23.711 18.914-42.625 42.621-42.625 23.711 0 42.625 18.914 42.625 42.625 0 14.742-5.9453 24.809-15.688 35.074-9.7461 10.266-23.262 19.555-35.816 29.598-3.3711 2.6992-5.3281 6.7812-5.3281 11.102v18.941c-0.054688 3.8047 1.4219 7.4688 4.0898 10.176 2.6719 2.7109 6.3164 4.2344 10.117 4.2344 3.8047 0 7.4492-1.5234 10.121-4.2344 2.668-2.707 4.1406-6.3711 4.0859-10.176v-11.988c10.352-7.9023 22.508-16.594 33.449-28.117 12.75-13.438 23.383-31.559 23.383-54.609 0-39.066-31.973-71.039-71.039-71.039zm0 198.91c-10.461 0-18.941 8.4805-18.941 18.941s8.4805 18.945 18.941 18.945c10.465 0 18.945-8.4844 18.945-18.945s-8.4805-18.941-18.945-18.941z" ]
              ]
      , HE.div [ HA.class' "explain-temporary-user" ]
              [ HE.p_ [ HE.text "Quick-sign up means users that just got started on MeroChat and have yet to finish creating their account" ]
              , HE.p_
                      [ HE.text "You can opt to not be seen (or messaged by) quick-sign up users on the "
                      , HE.a [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowSettings ] [ HE.text " settings" ]
                      , HE.text " page"
                      ]
              ]
      ]

from ∷ Suggestion → Array (Html ImMessage)
from suggestion = DM.maybe [] (\c → [ HE.span_ [ HE.span [ HA.class' "duller" ] [ HE.text "from " ], HE.span_ [ HE.text c ] ] ]) suggestion.country

speaks ∷ Suggestion → Array (Html ImMessage)
speaks suggestion
      | DA.null suggestion.languages = []
      | otherwise = [ HE.div_ $ HE.span [ HA.class' "duller" ] [ HE.text "speaks " ] : (DA.intersperse separator $ map (\c → HE.span_ [ HE.text c ]) suggestion.languages) ]

badges ∷ ∀ message. Array Badge → Array (Html message)
badges source = map (it <<< SB.badgeFor) source
      where
      it bf = HE.div [ HA.class' "badge", HA.title bf.description ] [ HE.img [ HA.width "18px", HA.height "18px", HA.class' "badge-img", HA.src $ SR.resourcePath (Left SR.Favicon) Ico ], HE.span [ HA.class' "badge-text" ] [ HE.text bf.text ] ]

profileContextMenu ∷ Int → Boolean → Array (Html ImMessage)
profileContextMenu id delete =
      [ HE.div [ HA.class' { "user-menu-item menu-item-heading": true, hidden: not delete }, HA.onClick <<< SpecialRequest <<< ToggleModal <<< Confirmation $ ConfirmDeleteChat id ] [ HE.text "Delete chat" ]
      , HE.div [ HA.class' "user-menu-item menu-item-heading", HA.onClick <<< SpecialRequest <<< ToggleModal <<< Confirmation $ ConfirmBlockUser id ] [ HE.text "Block" ]
      , HE.div [ HA.class' "user-menu-item menu-item-heading", HA.onClick <<< SpecialRequest <<< ToggleModal <<< Confirmation $ ConfirmReport id ] [ HE.text "Report" ]
      ]

welcomeTemporary ∷ ImModel → Html ImMessage
welcomeTemporary model = HE.div [ HA.class' "card-top-welcome-filter" ]
      [ HE.div [ HA.class' "card-top-header" ]
              [ HE.div [ HA.class' "welcome" ] [ HE.text $ "Welcome, " <> model.user.name ]
              , signUpCall model.user.joined
              ]
      , HE.div [ HA.class' "back-filter" ]
              [ SIA.arrow [ HA.class' "svg-back-profile hidden", HA.onClick $ ToggleInitialScreen true ]
              , onlineOnlyFilter model
              ]
      ]

signUpCall ∷ DateTimeWrapper → Html ImMessage
signUpCall joined = HE.div [ HA.class' "sign-up-call" ]
      [ HE.text "Enjoying MeroChat?"
      , HE.a [ HA.class' "warning-temporary bold", HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowProfile ] [ HE.text $ " Create an account  " <> remaining ]
      , HE.text " to keep your chats"
      ]
      where
      remaining = case DI.floor <<< SC.coerce $ SUR.temporaryUserExpiration joined of
            0 → " today"
            1 → " until tomorrow"
            n → " in " <> show n <> " days"

welcome ∷ ImModel → Html ImMessage
welcome model = HE.div [ HA.class' "card-top-welcome-filter" ]
      [ HE.div [ HA.class' "card-top-header" ]
              [ HE.div [ HA.class' "welcome" ] [ HE.text $ "Welcome, " <> model.user.name <> "!" ]
              , HE.div [ HA.class' "welcome-new" ] $
                      if not SP.hasPrivilege StartChats model.user then
                            [ HE.span [ HA.class' "no-self-start" ] [ CCP.notEnoughKarma "start chats" (SpecialRequest <<< ToggleModal $ Screen ShowKarmaPrivileges) ]
                            ]
                      else
                            case model.user.profileVisibility of
                                  Nobody → warn "hidden"
                                  Contacts → warn "contacts only"
                                  _ →
                                        [ HE.text "Here are your newest chat suggestions"

                                        ]
              ]
      , HE.div [ HA.class' "back-filter" ]
              [ SIA.arrow [ HA.class' "svg-back-profile hidden", HA.onClick $ ToggleInitialScreen true ]
              , onlineOnlyFilter model
              ]
      ]

      where
      warn level =
            [ HE.text $ "Your profile is set to " <> level <> ". Change your "
            , HE.a [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowSettings ] [ HE.text " settings " ]
            , HE.text "to see new chat suggestions"
            ]

onlineOnlyFilter ∷ ImModel → Html ImMessage
onlineOnlyFilter model =
      HE.div [ HA.class' { "online-only-filter": true, hidden: model.user.profileVisibility == Nobody } ]
            [ HE.input [ HA.type' "checkbox", HA.checked (model.suggestionsFrom == OnlineOnly), HA.id "online-only", HA.onClick ToggleSuggestionsFromOnline ]
            , HE.label [ HA.for "online-only", HA.class' "online-only-label" ] [ HE.text "Show only users online" ]
            ]

miniSuggestions ∷ ImModel → Html ImMessage
miniSuggestions model = HE.div [ HA.class' "mini-suggestions" ]
      case model.suggesting >>= (\sid → DA.find ((sid == _) <<< _.id) model.suggestions) of
            Just suggestion | not model.smallScreen && DM.isJust model.chatting && not model.showCollapsedMiniSuggestions →
                  [ HE.svg [ HA.class' "svg-32 svg-text-color", HA.viewBox "0 0 24 24", HA.onClick ToggleCollapsedMiniSuggestions ]
                          [ HE.path' [ HA.d "M18 12L12 18L6 12", HA.strokeWidth "2" ]
                          , HE.path' [ HA.d "M18 6L12 12L6 6", HA.strokeWidth "2" ]
                          ]

                  , seeAllSuggestions
                  , HE.div [ HA.class' "mini-suggestion-cards" ]
                          [ HE.div [ HA.class' "mini-avatar-info-arrows" ]
                                  [ arrow backArrow model.freeToFetchSuggestions [] $ SpecialRequest PreviousSuggestion
                                  , HE.div [ HA.class' "mini-avatar-info", HA.title "See full profile", HA.onClick <<< SpecialRequest <<< ToggleModal <<< Special $ ShowSuggestionCard suggestion.id ]
                                          [ HE.img [ HA.src $ SA.fromAvatar suggestion, HA.class' "mini-suggestion-avatar" ]
                                          , HE.div [ HA.class' "mini-suggestion-info" ]
                                                  ( [ HE.div_
                                                            [ HE.strong [ HA.class' "mini-suggestion-karma" ] [ HE.text $ SI.thousands suggestion.karma ]
                                                            , HE.span [ HA.class' "duller" ] [ HE.text $ " karma • #" <> show suggestion.karmaPosition ]
                                                            ]
                                                    ] <> genderAge suggestion
                                                          <> onlineStatus model.user suggestion
                                                  )
                                          ]
                                  , arrow nextArrow model.freeToFetchSuggestions [ "next-arrow" ] $ SpecialRequest NextSuggestion
                                  ]
                          , HE.div [ HA.class' "mini-name-options" ]
                                  [ HE.strong_ [ HE.text suggestion.name ]
                                  , HE.div [ HA.class' { "mini-options": true, hidden: suggestion.id == backerId } ]
                                          [ HE.svg [ HA.class' "svg-32 svg-mini-chat", HA.viewBox "0 0 24 24", HA.onClick $ if suggestion.isContact then ResumeSuggestionChat suggestion.id else ToggleMiniChatInput ]
                                                  [ HE.path' [ HA.d "M4.32698 6.63803L5.21799 7.09202L4.32698 6.63803ZM4.7682 20.2318L4.06109 19.5247H4.06109L4.7682 20.2318ZM18.362 16.673L18.816 17.564L18.816 17.564L18.362 16.673ZM19.673 15.362L20.564 15.816L20.564 15.816L19.673 15.362ZM19.673 6.63803L20.564 6.18404L20.564 6.18404L19.673 6.63803ZM18.362 5.32698L18.816 4.43597L18.816 4.43597L18.362 5.32698ZM5.63803 5.32698L6.09202 6.21799L5.63803 5.32698ZM7.70711 17.2929L7 16.5858L7.70711 17.2929ZM5 9.8C5 8.94342 5.00078 8.36113 5.03755 7.91104C5.07337 7.47262 5.1383 7.24842 5.21799 7.09202L3.43597 6.18404C3.18868 6.66937 3.09012 7.18608 3.04419 7.74817C2.99922 8.2986 3 8.97642 3 9.8H5ZM5 12V9.8H3V12H5ZM3 12V17H5V12H3ZM3 17V19.9136H5V17H3ZM3 19.9136C3 21.2054 4.56185 21.8524 5.4753 20.9389L4.06109 19.5247C4.40757 19.1782 5 19.4236 5 19.9136H3ZM5.4753 20.9389L8.41421 18L7 16.5858L4.06109 19.5247L5.4753 20.9389ZM15.2 16H8.41421V18H15.2V16ZM17.908 15.782C17.7516 15.8617 17.5274 15.9266 17.089 15.9624C16.6389 15.9992 16.0566 16 15.2 16V18C16.0236 18 16.7014 18.0008 17.2518 17.9558C17.8139 17.9099 18.3306 17.8113 18.816 17.564L17.908 15.782ZM18.782 14.908C18.5903 15.2843 18.2843 15.5903 17.908 15.782L18.816 17.564C19.5686 17.1805 20.1805 16.5686 20.564 15.816L18.782 14.908ZM19 12.2C19 13.0566 18.9992 13.6389 18.9624 14.089C18.9266 14.5274 18.8617 14.7516 18.782 14.908L20.564 15.816C20.8113 15.3306 20.9099 14.8139 20.9558 14.2518C21.0008 13.7014 21 13.0236 21 12.2H19ZM19 9.8V12.2H21V9.8H19ZM18.782 7.09202C18.8617 7.24842 18.9266 7.47262 18.9624 7.91104C18.9992 8.36113 19 8.94342 19 9.8H21C21 8.97642 21.0008 8.2986 20.9558 7.74817C20.9099 7.18608 20.8113 6.66937 20.564 6.18404L18.782 7.09202ZM17.908 6.21799C18.2843 6.40973 18.5903 6.71569 18.782 7.09202L20.564 6.18404C20.1805 5.43139 19.5686 4.81947 18.816 4.43597L17.908 6.21799ZM15.2 6C16.0566 6 16.6389 6.00078 17.089 6.03755C17.5274 6.07337 17.7516 6.1383 17.908 6.21799L18.816 4.43597C18.3306 4.18868 17.8139 4.09012 17.2518 4.04419C16.7014 3.99922 16.0236 4 15.2 4V6ZM8.8 6H15.2V4H8.8V6ZM6.09202 6.21799C6.24842 6.1383 6.47262 6.07337 6.91104 6.03755C7.36113 6.00078 7.94342 6 8.8 6V4C7.97642 4 7.2986 3.99922 6.74817 4.04419C6.18608 4.09012 5.66937 4.18868 5.18404 4.43597L6.09202 6.21799ZM5.21799 7.09202C5.40973 6.71569 5.71569 6.40973 6.09202 6.21799L5.18404 4.43597C4.43139 4.81947 3.81947 5.43139 3.43597 6.18404L5.21799 7.09202ZM8.41421 18V16C7.88378 16 7.37507 16.2107 7 16.5858L8.41421 18Z" ]
                                                  , HE.path' [ HA.d "M8 9L16 9", HA.strokeWidth "2", HA.strokeLinecap "round", HA.strokeLinejoin "round" ]
                                                  , HE.path' [ HA.d "M8 13L13 13", HA.strokeWidth "2", HA.strokeLinecap "round", HA.strokeLinejoin "round" ]
                                                  ]
                                          , HE.div [ HA.class' "outer-user-menu" ]
                                                  [ SIA.contextMenu $ show MiniSuggestionContextMenu
                                                  , HE.div [ HA.class' { "user-menu mini menu-up": true, visible: model.toggleContextMenu == ShowMiniSuggestionContextMenu } ] $ profileContextMenu suggestion.id false
                                                  ]
                                          ]
                                  ]
                          , HE.div [ HA.class' "mini-headline-tags", HA.title "See full profile", HA.onClick <<< SpecialRequest <<< ToggleModal <<< Special $ ShowSuggestionCard suggestion.id ]
                                  ( [ HE.div [ HA.class' "mini-headline" ] [ HE.text suggestion.headline ]
                                    , HE.hr' [ HA.class' "tag-ruler" ]
                                    ] <> map (\c → HE.span [ HA.class' "tag" ] [ HE.text c ]) suggestion.tags
                                  )

                          , HE.div [ HA.class' { "suggestion-input": true, hidden: not model.showMiniChatInput } ] [ SIVC.chatBarInput (Left suggestion.id) MiniChatInputSuggestion model ]
                          ]
                  ]
            Just suggestion | model.showCollapsedMiniSuggestions && not model.smallScreen && DM.isJust model.chatting →
                  [ HE.svg [ HA.class' "svg-32", HA.viewBox "0 0 24 24", HA.onClick ToggleCollapsedMiniSuggestions ]
                          [ HE.path' [ HA.d "M18 18L12 12L6 18", HA.stroke "#EFE5DC", HA.strokeWidth "2" ]
                          , HE.path' [ HA.d "M18 12L12 6L6 12", HA.stroke "#EFE5DC", HA.strokeWidth "2" ]
                          ]
                  , seeAllSuggestions
                  , HE.div [ HA.class' "mini-suggestion-cards collapsed" ]
                          [ HE.div [ HA.class' "mini-avatar-info-arrows" ]
                                  [ arrow backArrow model.freeToFetchSuggestions [] $ SpecialRequest PreviousSuggestion
                                  , HE.div [ HA.class' "mini-avatar-info", HA.title "See full profile", HA.onClick <<< SpecialRequest <<< ToggleModal <<< Special $ ShowSuggestionCard suggestion.id ]
                                          [ HE.img [ HA.src $ SA.fromAvatar suggestion, HA.class' "mini-suggestion-avatar" ]
                                          , HE.div [ HA.class' "mini-suggestion-info" ]
                                                  ( [ HE.strong [ HA.class' "collapsed-name" ] [ HE.text suggestion.name ]

                                                    ] <> onlineStatus model.user suggestion
                                                  )
                                          ]
                                  , arrow nextArrow model.freeToFetchSuggestions [ "next-arrow" ] $ SpecialRequest NextSuggestion
                                  ]
                          ]
                  ]
            _ → []

seeAllSuggestions ∷ Html ImMessage
seeAllSuggestions = HE.div [ HA.class' "see-all-suggestions" ]
      [ HE.strong [ HA.class' "mini-chat-label" ] [ HE.text "Chat suggestions" ]
      , HE.div [ HA.class' "see-all-home", HA.onClick ResumeSuggesting, HA.title "See all suggestions" ]
              [ home
              ]
      ]
