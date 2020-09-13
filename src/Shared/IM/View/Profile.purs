module Shared.IM.View.Profile where

import Prelude
import Shared.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple (Tuple(..))
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.Avatar as SA
import Shared.Markdown as SM
import Shared.Unsafe ((!@))

profile :: IMModel -> Html IMMessage
profile { suggestions, contacts, suggesting, chatting, fullContactProfileVisible } =
      case Tuple suggesting chatting of
            Tuple (Just index) Nothing -> suggestion suggesting (suggestions !@ index)
            Tuple Nothing (Just index) ->
                  if fullContactProfileVisible then
                        fullProfile false chatting (contacts !@ index).user
                  else
                        contact chatting (contacts !@ index).user
            _ -> HE.div (HA.class' "suggestion") <<< HE.div_ <<< HE.img $ HA.src "/client/media/logo.png"

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
            ],

            HE.div' [HA.class' "description-message hidden", HA.innerHTML (SM.toHTML description)]
      ]

suggestion :: Maybe Int -> Suggestion -> Html IMMessage
suggestion = fullProfile true

fullProfile :: Boolean -> Maybe Int -> IMUser -> Html IMMessage
fullProfile isSuggesting index { id, name, avatar, age, karma, headline, gender, country, languages, tags, description } =
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
                  -- HE.div_ [
                  --       HE.span [HA.class' "duller"] "Last seen",
                  --       HE.text " Yesterday"
                  -- ],
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

            HE.div' [HA.class' "description-message", HA.innerHTML (SM.toHTML description)]
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