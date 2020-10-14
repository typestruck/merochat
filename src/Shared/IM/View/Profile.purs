module Shared.IM.View.Profile where

import Prelude
import Shared.Types

import Data.Array ((!!), (..))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple (Tuple(..))
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.Avatar as SA
import Shared.Markdown as SM
import Shared.Options.Profile (tagsSmallCard)
import Shared.Unsafe ((!@))

profile :: Boolean -> IMModel -> Html IMMessage
profile isClientRender model@{ suggestions, contacts, suggesting, chatting, fullContactProfileVisible, suggestionCard } =
      if DA.null suggestions then
            emptySuggestions
       else
            case Tuple suggesting chatting of
                  Tuple (Just index) Nothing -> suggestion isClientRender model index
                  Tuple Nothing (Just index) ->
                        if fullContactProfileVisible then
                              fullProfile isClientRender false chatting (contacts !@ index).user
                        else
                              contact chatting (contacts !@ index).user
                  _ -> emptySuggestions
      where emptySuggestions = HE.div (HA.class' "suggestion") <<< HE.div_ <<< HE.img $ HA.src "/client/media/logo.png"

contact :: Maybe Int -> IMUser -> Html IMMessage
contact chatting { id, name, avatar, age, karma, headline, gender, country, languages, tags, description } =
      HE.div [HA.class' "profile-contact", HA.title "Click to see full profile", HA.onClick ToggleContactProfile] [
            HE.div (HA.class' "profile-contact-top") [
                  HE.img [HA.class' $ "avatar-profile " <> SA.avatarColorClass chatting, HA.src $ SA.avatarForRecipient chatting avatar],
                  HE.div (HA.class' "profile-contact-header") [
                        HE.h1_ name,
                        HE.div (HA.class' "headline") headline,
                        HE.div (HA.class' "contact-profile-tags") $ map toTagSpan tags
                  ],
                  HE.div (HA.class' "profile-contact-deets") [
                        HE.div_ [
                              HE.span [HA.class' "span-info"] $ show karma,
                              HE.span [HA.class' "duller"] " karma"
                        ],
                        HE.div_ [
                              toSpan $ map show age,
                              duller (DM.isNothing age || DM.isNothing gender) ", ",
                              toSpan gender
                        ],
                        HE.div_ [
                              duller (DM.isNothing country) "from ",
                              toSpan country
                        ],
                        HE.div_ ([
                              duller (DA.null languages) "speaks "
                        ] <> (DA.intercalate [duller false ", "] $ map (DA.singleton <<< spanWith) languages))
                  ]
            ]
      ]

suggestion :: Boolean -> IMModel -> Int -> Html IMMessage
suggestion isClientRender { suggestionCard, user, suggestions } index =
      case suggestionCard of
            SmallCard ->
                  HE.div (HA.class' "suggestion-cards") [
                        HE.div (HA.class' "card-top-header") [
                              HE.div (HA.class' "welcome") $ "Welcome, " <> user.name,
                              HE.div (HA.class' "welcome-new") "Here are your newest chat suggestions"
                        ],
                        HE.div (HA.class' "small-cards") $ map (\i -> card (suggestions !! i) i) (index .. (index + 2))
                  ]
            BigCard -> fullProfile isClientRender true (Just index) (suggestions !@ index)
      where card suggestion suggestionIndex =
                  case suggestion of
                        Nothing -> HE.createEmptyElement "div"
                        Just { name, avatar, age, headline, karma, gender, country, tags } ->
                              let   isCenter = suggestionIndex == index + 1
                                    maybeSuggestionIndex = Just suggestionIndex
                                    attrs
                                          | isCenter = [ HA.class' "card card-center", HA.onClick $ ToggleSuggestionCard maybeSuggestionIndex]
                                          | otherwise = [HA.class' "card card-sides"]
                              in HE.div attrs [
                                    HE.div (HA.class' "card-top") [
                                          HE.img [HA.class' $ "avatar-profile " <> SA.avatarColorClass maybeSuggestionIndex, HA.src $ SA.avatarForRecipient maybeSuggestionIndex avatar],
                                          HE.h1 (HA.class' "name") name,
                                          HE.div (HA.class' "headline") headline,
                                          HE.div_ [
                                                HE.span [HA.class' "span-info"] $ show karma,
                                                HE.span [HA.class' "duller"] " karma"
                                          ],
                                          HE.div (HA.class' "profile-asl") [

                                                HE.div_ [
                                                      toSpan $ map show age,
                                                      duller (DM.isNothing age || DM.isNothing gender) ", ",
                                                      toSpan gender
                                                ],
                                                HE.div_ [
                                                      duller (DM.isNothing country) "from ",
                                                      toSpan country
                                                ]
                                          ],
                                          HE.div (HA.class' "profile-tags") <<< map toTagSpan $ DA.take tagsSmallCard tags,
                                          arrow PreviousSuggestion,
                                          arrow NextSuggestion
                                    ],
                                    HE.div (HA.class' {"card-more": true, hidden: not isCenter}) "View complete profile"
                              ]

            arrow message = HE.div [HA.class' "suggestion-arrow", HA.onClick message] [
                  case message of
                        PreviousSuggestion ->
                              HE.svg [HA.class' "svg-32", HA.viewBox "0 0 512 512"] [
                                    HE.circle' [HA.cx "256", HA.cy "256", HA.r "240", HA.opacity "0.25"],
                                    HE.polygon' [HA.points "276.149 343.518 209.63 276.999 385 276.999 385 235 209.631 235 276.149 168.482 246.45 138.784 129.234 256 246.45 373.217 276.149 343.518"]
                              ]
                        _ ->
                              HE.svg [HA.class' "svg-32", HA.viewBox "0 0 512 512"] [
                                    HE.circle' [HA.cx "256", HA.cy "256", HA.r "240", HA.opacity "0.25"],
                                    HE.polygon' [HA.points "235.851 343.518 265.55 373.217 382.766 256 265.55 138.784 235.851 168.482 302.369 235 127.887 235 127.887 276.999 302.37 276.999 235.851 343.518"]
                              ]
            ]

fullProfile :: Boolean -> Boolean -> Maybe Int -> IMUser -> Html IMMessage
fullProfile isClientRender isSuggesting index { id, name, avatar, age, karma, headline, gender, country, languages, tags, description } =
      HE.div attrs [
            HE.div (HA.class' "ss") [
                  HE.a [HA.class' "skip", HA.title "See previous profile again", HA.onClick PreviousSuggestion] [
                  HE.svg [HA.id "cil-arrow-thick-from-right", HA.viewBox "0 0 24 24", HA.class' "svg-50"] [
                              HE.path' $ HA.d "M11.936 2.625h-1.811l-9.375 9.384 9.375 9.366h1.81v-5.625h6.75v-7.5h-6.75zM17.186 9.75v4.5h-6.75v5.315l-7.564-7.557 7.564-7.572v5.314z",
                              HE.path' $ HA.d "M21.686 2.625h1.5v18.75h-1.5v-18.75z"
                        ]
                  ],
                  HE.a [HA.class' "skip green", HA.title "See next profile", HA.onClick NextSuggestion] [
                        HE.svg [HA.id "cil-arrow-thick-from-left", HA.class' "svg-50", HA.viewBox "0 0 24 24"] [
                              HE.path' $ HA.d "M13.875 2.625h-1.811v5.625h-6.75v7.5h6.75v5.625h1.81l9.375-9.366zM13.564 19.565v-5.315h-6.75v-4.5h6.75v-5.314l7.564 7.572z",
                              HE.path' $ HA.d "M0.814 2.625h1.5v18.75h-1.5v-18.75z"
                        ]
                  ]
            ],

            HE.img [HA.class' $ "avatar-profile " <> SA.avatarColorClass index, HA.src $ SA.avatarForRecipient index avatar],
            HE.h1_ name,
            HE.div (HA.class' "headline") headline,
            HE.div (HA.class' "profile-karma") [
                  HE.div_ [
                        HE.span [HA.class' "span-info"] $ show karma,
                        HE.span [HA.class' "duller"] " karma"
                  ]
            ],

            HE.div (HA.class' "profile-asl") [
                  HE.div_ [
                        toSpan $ map show age,
                        duller (DM.isNothing age || DM.isNothing gender) ", ",
                        toSpan gender
                  ],
                  HE.div_ [
                        duller (DM.isNothing country) "from ",
                        toSpan country
                  ],
                  HE.div_ ([
                        duller (DA.null languages) "speaks "
                  ] <> (DA.intercalate [duller false ", "] $ map (DA.singleton <<< spanWith) languages))
            ],

            HE.div (HA.class' "profile-tags") $ map toTagSpan tags,

            duller false "About",
                  -- HE.div_ $ HE.button [HA.class' "action-button", HA.onClick $ BlockUser id] "Block"

            SM.displayMarkdown { extraClasses: "description-message", markdown: description, useHooks: isClientRender }
      ]
      where attrs = if isSuggesting then [HA.class' "suggestion new"] else [HA.class' "suggestion old", HA.title "Click to hide full profile", HA.onClick ToggleContactProfile]

toSpan :: Maybe String -> Html IMMessage
toSpan =
      case _ of
            Just s -> spanWith s
            _ -> HE.createEmptyElement "span"

spanWith :: String -> Html IMMessage
spanWith = HE.span (HA.class' "span-info")

toTagSpan :: String -> Html IMMessage
toTagSpan = HE.span (HA.class' "tag")

duller :: Boolean -> String -> Html IMMessage
duller hidden t = HE.span (HA.class' { "duller" : true, "hidden": hidden }) t