module Shared.Im.View.LogoMenu where

import Prelude
import Shared.Im.Types

import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.Element (ElementId(..))
import Shared.Im.Svg (backArrow, nextArrow)
import Shared.Im.Svg as SIA
import Shared.Im.Svg as SIS
import Shared.Im.View.ChatInput as SIVC
import Shared.Im.View.SuggestionProfile as SISP
import Shared.Intl as SI
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SP
import Web.HTML.Event.EventTypes (online)

logoMenu ∷ ImModel → Html ImMessage
logoMenu model = HE.div (HA.class' "relative")
      [ HE.div (HA.class' "tabs")
              [ HE.div (HA.class' "tab-item duller")
                      [ HE.svg [ HA.class' "svg-32", HA.viewBox "0 0 16 16" ]
                              [ HE.path' [ HA.class' "strokeless", HA.d "M2.29,11.1l.45-.55H13.19a1.1,1.1,0,0,0,1-1.18V1.1A1,1,0,0,0,13.27,0H.94A1,1,0,0,0,0,1.11v9.28H0v3.5l1.4-1.7.06-.08.73-.89Zm-.95-.43-.11.13L1,11.1v-10s0,0,0,0H13.22s0,0,0,0V9.37a.42.42,0,0,1,0,.11H2.32l0,0-.1.12L2,9.83Z" ]
                              , HE.path' [ HA.class' "strokeless", HA.d "M16,12.74V4.11a1,1,0,0,0-1-1h-.52v1H15s0,0,0,0v9.3l-.26-.28-.82-.91L13.74,12l-.13-.14H2.86l-.86,1H13.17l.57.63.81.9L16,16V12.74Z" ]
                              , HE.rect' [ HA.class' "strokeless", HA.x "3.81", HA.y "2.94", HA.width "7.23", HA.height "0.75" ]
                              , HE.rect' [ HA.class' "strokeless", HA.x "3.81", HA.y "4.69", HA.width "7.23", HA.height "0.75" ]
                              , HE.rect' [ HA.class' "strokeless", HA.x "3.81", HA.y "6.44", HA.width "7.23", HA.height "0.75" ]
                              ]
                      , HE.text "Chats"
                      ]
              , HE.div [ HA.class' "tab-item duller", HA.onClick $ ToggleInitialScreen false ]
                      [ HE.svg [ HA.class' "svg-32 svg-duller", HA.viewBox "0 0 16 16" ]
                              [ HE.path' [ HA.class' "strokeless", HA.d "M2.29,11.1l.45-.55H13.19a1.1,1.1,0,0,0,1-1.18V1.1A1,1,0,0,0,13.27,0H.94A1,1,0,0,0,0,1.11v9.28H0v3.5l1.39-1.7.06-.08.73-.89Zm-.95-.43-.11.13L1,11.1v-10s0,0,0,0H13.22s0,0,0,0V9.37a.42.42,0,0,1,0,.11H2.32l0,0-.1.12L2,9.83Z" ]
                              , HE.path' [ HA.class' "strokeless", HA.d "M16,12.74V4.11a1,1,0,0,0-1-1h-.52v1H15s0,0,0,0v9.3l-.26-.28-.82-.91L13.74,12l-.13-.14H2.86l-.86,1H13.17l.57.63.81.9L16,16V12.74Z" ]
                              , HE.path' [ HA.class' "strokeless", HA.d "M7.23,8.41a5.5,5.5,0,0,0,5.49-5.27l0-1h-11v1a5.46,5.46,0,0,0,1.6,3.64A5.54,5.54,0,0,0,7.23,8.41Zm4.49-5.25A4.5,4.5,0,0,1,7.23,7.41,4.57,4.57,0,0,1,4,6.08a4.44,4.44,0,0,1-1.3-2.92Z" ]
                              , HE.path' [ HA.class' "strokeless", HA.d "M4.86,5.06a.52.52,0,0,0,.53-.53A.53.53,0,0,0,4.86,4a.53.53,0,0,0-.53.53A.53.53,0,0,0,4.86,5.06Z" ]
                              , HE.path' [ HA.class' "strokeless", HA.d "M7.23,5.06a.52.52,0,0,0,.53-.53.53.53,0,1,0-1,0A.52.52,0,0,0,7.23,5.06Z" ]
                              , HE.path' [ HA.class' "strokeless", HA.d "M9.61,5.06a.53.53,0,0,0,.53-.53A.53.53,0,0,0,9.61,4a.53.53,0,0,0-.53.53A.52.52,0,0,0,9.61,5.06Z" ]
                              ]
                      , HE.text "Suggestions"
                      ]
              ]
      , HE.div (HA.class' { fortune: true, hidden: DM.isNothing model.fortune })
              [ HE.div (HA.class' "fortune-deets")
                      [ HE.text $ DM.fromMaybe "" model.fortune
                      ]
              , HE.svg [ HA.viewBox "0 0 512 512", HA.onClick (ToggleFortune false) ]
                      [ HE.title "Close"
                      , HE.polygon' $ HA.points "438.627 118.627 393.373 73.373 256 210.746 118.627 73.373 73.373 118.627 210.746 256 73.373 393.373 118.627 438.627 256 301.254 393.373 438.627 438.627 393.373 301.254 256 438.627 118.627"
                      ]
              ]
      , HE.div [ HA.class' "logo-contact-list" ]
              [ miniSuggestions model
              , HE.img
                      [ HA.onDblclick $ ToggleFortune true
                      , HA.createAttribute "srcset" $ DS.joinWith " " [ SP.resourcePath (Left InvertedLogo) Svg, "180w,", SP.resourcePath (Left LogoSmall) Png, "210w" ]
                      , HA.createAttribute "sizes" "(max-width: 1920px) 180px, 210px"
                      , HA.src $ SP.resourcePath (Left Logo) Png
                      ]
              ]
      ]

miniSuggestions ∷ ImModel → Html ImMessage
miniSuggestions model = HE.div (HA.class' "mini-suggestions")
      case model.suggestions !! model.suggesting of
            Just suggestion | not model.smallScreen && DM.isJust model.chatting && not model.showCollapsedMiniSuggestions →
                  [ HE.svg [ HA.class' "svg-32", HA.viewBox "0 0 24 24", HA.onClick ToggleCollapsedMiniSuggestions ]
                          [ HE.path' [ HA.d "M18 12L12 18L6 12", HA.strokeWidth "2" ]
                          , HE.path' [ HA.d "M18 6L12 12L6 6", HA.strokeWidth "2" ]
                          ]

                  , seeAllSuggestions
                  , HE.div (HA.class' "mini-suggestion-cards")
                          [ HE.div (HA.class' "mini-avatar-info-arrows")
                                  [ arrow backArrow $ SpecialRequest PreviousSuggestion
                                  , HE.div [ HA.class' "mini-avatar-info", HA.title "See full profile", HA.onClick ResumeSuggesting ]
                                          [ HE.img [ HA.src $ SA.fromAvatar suggestion.avatar, HA.class' "mini-suggestion-avatar" ]
                                          , HE.div (HA.class' "mini-suggestion-info")
                                                  ( [ HE.div_
                                                            [ HE.strong (HA.class' "mini-suggestion-karma") $ SI.thousands suggestion.karma
                                                            , HE.span (HA.class' "duller") $ " karma • #" <> show suggestion.karmaPosition
                                                            ]
                                                    ] <> genderAge suggestion
                                                          <> onlineStatus suggestion
                                                  )
                                          ]
                                  , arrow nextArrow $ SpecialRequest NextSuggestion
                                  ]
                          , HE.div (HA.class' "mini-name-options")
                                  [ HE.strong_ suggestion.name
                                  , HE.div [ HA.class' "mini-options" ]
                                          [ HE.svg [ HA.class' "svg-32 svg-mini-chat", HA.viewBox "0 0 24 24", HA.onClick ToggleMiniChatInput ]
                                                  [ HE.path' [ HA.d "M4.32698 6.63803L5.21799 7.09202L4.32698 6.63803ZM4.7682 20.2318L4.06109 19.5247H4.06109L4.7682 20.2318ZM18.362 16.673L18.816 17.564L18.816 17.564L18.362 16.673ZM19.673 15.362L20.564 15.816L20.564 15.816L19.673 15.362ZM19.673 6.63803L20.564 6.18404L20.564 6.18404L19.673 6.63803ZM18.362 5.32698L18.816 4.43597L18.816 4.43597L18.362 5.32698ZM5.63803 5.32698L6.09202 6.21799L5.63803 5.32698ZM7.70711 17.2929L7 16.5858L7.70711 17.2929ZM5 9.8C5 8.94342 5.00078 8.36113 5.03755 7.91104C5.07337 7.47262 5.1383 7.24842 5.21799 7.09202L3.43597 6.18404C3.18868 6.66937 3.09012 7.18608 3.04419 7.74817C2.99922 8.2986 3 8.97642 3 9.8H5ZM5 12V9.8H3V12H5ZM3 12V17H5V12H3ZM3 17V19.9136H5V17H3ZM3 19.9136C3 21.2054 4.56185 21.8524 5.4753 20.9389L4.06109 19.5247C4.40757 19.1782 5 19.4236 5 19.9136H3ZM5.4753 20.9389L8.41421 18L7 16.5858L4.06109 19.5247L5.4753 20.9389ZM15.2 16H8.41421V18H15.2V16ZM17.908 15.782C17.7516 15.8617 17.5274 15.9266 17.089 15.9624C16.6389 15.9992 16.0566 16 15.2 16V18C16.0236 18 16.7014 18.0008 17.2518 17.9558C17.8139 17.9099 18.3306 17.8113 18.816 17.564L17.908 15.782ZM18.782 14.908C18.5903 15.2843 18.2843 15.5903 17.908 15.782L18.816 17.564C19.5686 17.1805 20.1805 16.5686 20.564 15.816L18.782 14.908ZM19 12.2C19 13.0566 18.9992 13.6389 18.9624 14.089C18.9266 14.5274 18.8617 14.7516 18.782 14.908L20.564 15.816C20.8113 15.3306 20.9099 14.8139 20.9558 14.2518C21.0008 13.7014 21 13.0236 21 12.2H19ZM19 9.8V12.2H21V9.8H19ZM18.782 7.09202C18.8617 7.24842 18.9266 7.47262 18.9624 7.91104C18.9992 8.36113 19 8.94342 19 9.8H21C21 8.97642 21.0008 8.2986 20.9558 7.74817C20.9099 7.18608 20.8113 6.66937 20.564 6.18404L18.782 7.09202ZM17.908 6.21799C18.2843 6.40973 18.5903 6.71569 18.782 7.09202L20.564 6.18404C20.1805 5.43139 19.5686 4.81947 18.816 4.43597L17.908 6.21799ZM15.2 6C16.0566 6 16.6389 6.00078 17.089 6.03755C17.5274 6.07337 17.7516 6.1383 17.908 6.21799L18.816 4.43597C18.3306 4.18868 17.8139 4.09012 17.2518 4.04419C16.7014 3.99922 16.0236 4 15.2 4V6ZM8.8 6H15.2V4H8.8V6ZM6.09202 6.21799C6.24842 6.1383 6.47262 6.07337 6.91104 6.03755C7.36113 6.00078 7.94342 6 8.8 6V4C7.97642 4 7.2986 3.99922 6.74817 4.04419C6.18608 4.09012 5.66937 4.18868 5.18404 4.43597L6.09202 6.21799ZM5.21799 7.09202C5.40973 6.71569 5.71569 6.40973 6.09202 6.21799L5.18404 4.43597C4.43139 4.81947 3.81947 5.43139 3.43597 6.18404L5.21799 7.09202ZM8.41421 18V16C7.88378 16 7.37507 16.2107 7 16.5858L8.41421 18Z" ]
                                                  , HE.path' [ HA.d "M8 9L16 9", HA.strokeWidth "2", HA.strokeLinecap "round", HA.strokeLinejoin "round" ]
                                                  , HE.path' [ HA.d "M8 13L13 13", HA.strokeWidth "2", HA.strokeLinecap "round", HA.strokeLinejoin "round" ]
                                                  ]
                                          , HE.div [ HA.class' "outer-user-menu" ]
                                                  [ SIA.contextMenu $ show MiniSuggestionContextMenu
                                                  , HE.div [ HA.class' { "user-menu mini menu-up": true, visible: model.toggleContextMenu == ShowMiniSuggestionContextMenu } ] $ SISP.profileContextMenu suggestion.id false
                                                  ]
                                          ]
                                  ]
                          , HE.div [ HA.class' "mini-headline-tags", HA.title "See full profile", HA.onClick ResumeSuggesting ]
                                  ( [ HE.div (HA.class' "mini-headline") suggestion.headline
                                    , HE.hr' (HA.class' "tag-ruler")
                                    ] <> map (HE.span (HA.class' "tag")) suggestion.tags
                                  )
                          , HE.div [ HA.class' { "suggestion-input": true, hidden: not model.showMiniChatInput } ] $ SIVC.chatBarInput MiniChatInputSuggestion model
                          ]
                  ]
            Just suggestion | model.showCollapsedMiniSuggestions && not model.smallScreen && DM.isJust model.chatting →
                  [ HE.svg [ HA.class' "svg-32", HA.viewBox "0 0 24 24", HA.onClick ToggleCollapsedMiniSuggestions ]
                          [ HE.path' [ HA.d "M18 18L12 12L6 18", HA.stroke "#EFE5DC", HA.strokeWidth "2" ]
                          , HE.path' [ HA.d "M18 12L12 6L6 12", HA.stroke "#EFE5DC", HA.strokeWidth "2" ]
                          ]
                  , seeAllSuggestions
                  , HE.div (HA.class' "mini-suggestion-cards collapsed")
                          [ HE.div (HA.class' "mini-avatar-info-arrows")
                                  [ arrow backArrow $ SpecialRequest PreviousSuggestion
                                  , HE.div [ HA.class' "mini-avatar-info", HA.title "See full profile", HA.onClick ToggleCollapsedMiniSuggestions ]
                                          [ HE.img [ HA.src $ SA.fromAvatar suggestion.avatar, HA.class' "mini-suggestion-avatar" ]
                                          , HE.div (HA.class' "mini-suggestion-info")
                                                  ( [ HE.strong (HA.class' "collapsed-name") suggestion.name

                                                    ] <> onlineStatus suggestion
                                                  )
                                          ]
                                          , arrow nextArrow $ SpecialRequest NextSuggestion
                                  ]
                          ]
                  ]
            _ → []
      where
      arrow svg message = HE.div (HA.class' "suggestion-arrow" : if model.freeToFetchSuggestions then [ HA.onClick message ] else []) svg

      genderAge suggestion =
            case DM.maybe [] (DA.singleton <<< HE.span_) suggestion.gender <> DM.maybe [] (DA.singleton <<< HE.span_ <<< show) suggestion.age of
                  [ g, a ] → [ HE.div_ [ g, HE.span (HA.class' "duller") ", ", a ] ]
                  ga → ga

      onlineStatus suggestion
            | not model.user.onlineStatus || not suggestion.onlineStatus = []
            | otherwise = [ HE.span_ $ show suggestion.availability ]

seeAllSuggestions ∷ Html ImMessage
seeAllSuggestions = HE.div (HA.class' "see-all-suggestions")
      [ HE.strong (HA.class' "mini-chat-label") "Chat suggestions"
      , HE.div [ HA.class' "see-all-home", HA.onClick ResumeSuggesting, HA.title "See all suggestions" ]
              [ SIS.home
              ]
      ]