module Shared.IM.View.Profile where

import Prelude
import Shared.Types

import Data.Array ((!!), (..))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Debug.Trace (spy)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.IM.View.Chat as SIVC
import Shared.Markdown as SM
import Shared.Unsafe ((!@))

profile :: IMModel -> Html IMMessage
profile model@{ suggestions, contacts, suggesting, chatting, fullContactProfileVisible } =
      if DA.null suggestions then
            emptySuggestions
       else
            case suggesting, chatting of
                  (Just index), Nothing -> suggestion model index
                  Nothing, i@(Just index) ->
                        if fullContactProfileVisible then
                              fullProfile FullContactProfile i model (contacts !@ index).user
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
                        HE.h1 (HA.class' "contact-name") name,
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
                        HE.div_  ([
                              duller (DA.null languages) "speaks "
                        ] <> (DA.intercalate [duller false ", "] $ map (DA.singleton <<< spanWith) languages))
                  ]
            ]
      ]

suggestion :: IMModel -> Int -> Html IMMessage
suggestion model@{ user, suggestions } index =
      HE.div (HA.class' "suggestion-cards") [
            HE.div (HA.class' "card-top-header") [
                  HE.div (HA.class' "welcome") $ "Welcome, " <> user.name,
                  HE.div (HA.class' "welcome-new") "Here are your newest chat suggestions"
            ],
            HE.div (HA.class' "cards") <<< map (\i -> card i $ suggestions !! i) $ (index - 1) .. (index + 1)
      ]
      where noSuggestion i = suggestions !! i == Nothing

            card suggestionIndex =
                  case _ of
                        Nothing -> HE.div' [HA.class' "card card-sides faded invisible"]
                        Just suggestion ->
                              let   isCenter = suggestionIndex == index
                                    attrs
                                          | isCenter = [ HA.class' {"card card-center" : true, "hide-previous-arrow" : noSuggestion (index - 1), "hide-next-arrow": noSuggestion (index + 1)}]
                                          | otherwise = [HA.class' "card card-sides faded" ]
                              in HE.div attrs $ fullProfile (if isCenter then CurrentSuggestion else OtherSuggestion) (Just suggestionIndex) model suggestion


arrow :: IMMessage -> Html IMMessage
arrow message = HE.div [HA.class' "suggestion-arrow", HA.onClick message] [
      case message of
            PreviousSuggestion ->
                  HE.svg [HA.class' "svg-55", HA.viewBox "0 0 55 55"] [
                        HE.path' [HA.class' "strokeless", HA.d "M54.6758 27.4046C54.6758 42.456 42.483 54.6576 27.4423 54.6576C12.4016 54.6576 0.20874 42.456 0.20874 27.4046C0.20874 12.3532 12.4016 0.151611 27.4423 0.151611C42.483 0.151611 54.6758 12.3532 54.6758 27.4046Z", HA.fill "#1B2921"],
                        HE.path' [HA.class' "filless", HA.strokeWidth "2.91996", HA.d "M32.2858 13.3713L19.6558 27.6094L32.2858 40.2293"]
                  ]
            _ ->
                  HE.svg [HA.class' "svg-55", HA.viewBox "0 0 38 38"] [
                        HE.path' [HA.class' "strokeless", HA.d "M4.57764e-05 19.0296C4.57764e-05 29.4062 8.41191 37.8181 18.7885 37.8181C29.165 37.8181 37.5769 29.4062 37.5769 19.0296C37.5769 8.65308 29.165 0.241211 18.7885 0.241211C8.41191 0.241211 4.57764e-05 8.65308 4.57764e-05 19.0296Z", HA.fill "#1B2921"],
                        HE.path' [HA.class' "filless",  HA.strokeWidth "2.01305", HA.d "M15.4472 9.35498L24.1606 19.1708L15.4472 27.8711"]
                  ]
]

fullProfile :: ProfilePresentation -> Maybe Int -> IMModel -> IMUser -> Html IMMessage
fullProfile presentation index model { id, name, avatar, age, karma, headline, gender, country, languages, tags, description } =
      case presentation of
            FullContactProfile -> HE.div [HA.class' "suggestion old", HA.title "Click to hide full profile", HA.onClick ToggleContactProfile] profile
            CurrentSuggestion -> HE.div [HA.class' "suggestion-center"] [
                  HE.div [HA.class' "suggestion new"]  profile,
                  HE.div [HA.class' "suggestion-input"] $ SIVC.chatBarInput model
            ]
            OtherSuggestion -> HE.div [HA.class' "suggestion new"] profile
      where profile = [
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

                  HE.div (HA.class' "tags-description") [
                        HE.div (HA.class' "profile-tags") $ map toTagSpan tags,

                        HE.span (HA.class' "duller profile-description-about" ) "About",
                              -- HE.div_ $ HE.button [HA.class' "action-button", HA.onClick $ BlockUser id] "Block"

                        HE.div' [HA.class' "description-message", HA.innerHtml $ SM.parse description]
                  ],
                  arrow PreviousSuggestion,
                  arrow NextSuggestion
            ]

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