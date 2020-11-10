module Shared.IM.View.Profile where

import Prelude
import Shared.Types

import Data.Array ((!!), (..))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.Markdown as SM
import Shared.Options.Profile (tagsSmallCard)
import Shared.Unsafe ((!@))

profile :: IMModel -> Html IMMessage
profile model@{ suggestions, contacts, suggesting, chatting, fullContactProfileVisible } =
      if DA.null suggestions then
            emptySuggestions
       else
            case suggesting, chatting of
                  (Just index), Nothing -> suggestion model index
                  Nothing, (Just index) ->
                        if fullContactProfileVisible then
                              fullProfile false chatting (contacts !@ index).user
                        else
                              contact chatting (contacts !@ index).user
                  _, _ -> emptySuggestions
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

suggestion :: IMModel -> Int -> Html IMMessage
suggestion { user, suggestions } index =
      HE.div (HA.class' "suggestion-cards") [
            HE.div (HA.class' "card-top-header") [
                  HE.div (HA.class' "welcome") $ "Welcome, " <> user.name,
                  HE.div (HA.class' "welcome-new") "Here are your newest chat suggestions"
            ],
            HE.div (HA.class' "cards") $ map (\i -> card (suggestions !! i) i) (index .. (index + 2))
      ]
      where card suggestion suggestionIndex =
                  case suggestion of
                        Nothing -> HE.createEmptyElement "div"
                        Just suggestion ->
                              let   isCenter = suggestionIndex == index + 1
                                    attrs
                                          | isCenter = [ HA.class' "card card-center"]
                                          | otherwise = [HA.class' "card card-sides"]
                              in HE.div attrs $ fullProfile true (Just suggestionIndex) suggestion

arrow :: IMMessage -> Html IMMessage
arrow message = HE.div [HA.class' "suggestion-arrow", HA.onClick message] [
      case message of
            PreviousSuggestion ->
                  HE.svg [HA.class' "svg-50", HA.viewBox "0 0 512 512"] [
                        HE.circle' [HA.cx "256", HA.cy "256", HA.r "240", HA.opacity "0.25"],
                        HE.polygon' [HA.points "276.149 343.518 209.63 276.999 385 276.999 385 235 209.631 235 276.149 168.482 246.45 138.784 129.234 256 246.45 373.217 276.149 343.518"]
                  ]
            _ ->
                  HE.svg [HA.class' "svg-50", HA.viewBox "0 0 512 512"] [
                        HE.circle' [HA.cx "256", HA.cy "256", HA.r "240", HA.opacity "0.25"],
                        HE.polygon' [HA.points "235.851 343.518 265.55 373.217 382.766 256 265.55 138.784 235.851 168.482 302.369 235 127.887 235 127.887 276.999 302.37 276.999 235.851 343.518"]
                  ]
]

fullProfile :: Boolean -> Maybe Int -> IMUser -> Html IMMessage
fullProfile isSuggesting index { id, name, avatar, age, karma, headline, gender, country, languages, tags, description } =
      HE.div attrs [
            HE.img [HA.class' $ "avatar-profile " <> SA.avatarColorClass index, HA.src $ SA.avatarForRecipient index avatar],
            HE.h1 (HA.class' "profile-name") name,
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

            HE.span (HA.class' "duller profile-description-about" ) "About",
                  -- HE.div_ $ HE.button [HA.class' "action-button", HA.onClick $ BlockUser id] "Block"

            HE.div' [HA.class' "description-message", HA.innerHtml $ SM.parse description],
            arrow PreviousSuggestion,
            arrow NextSuggestion
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