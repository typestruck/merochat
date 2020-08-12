module Shared.IM.View.Suggestion where

import Prelude
import Shared.IM.Types

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Data.String as DS
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.Avatar as SA

profile :: IMModel -> Maybe IMUser -> Html IMMessage
profile (IMModel { suggesting, chatting }) =
      case _ of
            Just (IMUser { id, name, avatar, age, karma, headline, gender, country, languages, tags }) ->
                  HE.div (HA.class' "suggestion") [
                        HE.a [HA.class' "skip", HA.title "See previous profile again", HA.onClick PreviousSuggestion] [
                              HE.svg [HA.id "cil-arrow-thick-from-right", HA.viewBox "0 0 24 24", HA.class' "svg-50"] [
                                    HE.path' $ HA.d "M11.936 2.625h-1.811l-9.375 9.384 9.375 9.366h1.81v-5.625h6.75v-7.5h-6.75zM17.186 9.75v4.5h-6.75v5.315l-7.564-7.557 7.564-7.572v5.314z",
                                    HE.path' $ HA.d "M21.686 2.625h1.5v18.75h-1.5v-18.75z"
                              ]
                        ],
                        HE.div (HA.class' "profile-info") [
                              HE.div_ $ HE.img [HA.class' "avatar-profile", HA.src $ SA.avatarForRecipient (chatting <|> suggesting) avatar],
                              HE.div_ [
                                    HE.h1_ name,
                                    HE.h3 (HA.class' "headline") headline
                              ],
                              HE.div_ $
                                    toInfoSpan false (map ((_ <> ",") <<< show) age) <>
                                    toInfoSpan true gender <>
                                    toInfoSpan true country <>
                                    --maybe include local time?
                                    (toInfoSpan false <<< maybeLanguages $ DS.joinWith ", " languages),
                              HE.div (HA.class' "karma-stats") <<< HE.span_ $ "Karma: " <> show karma,
                              HE.div_ $ map toTagSpan tags,
                              HE.div_ $ HE.button [HA.class' "action-button", HA.onClick $ BlockUser id] "Block"
                        ],
                        HE.a [HA.class' "skip green", HA.title "See next profile", HA.onClick NextSuggestion] [
                              HE.svg [HA.id "cil-arrow-thick-from-left", HA.class' "svg-50", HA.viewBox "0 0 24 24"] [
                                    HE.path' $ HA.d "M13.875 2.625h-1.811v5.625h-6.75v7.5h6.75v5.625h1.81l9.375-9.366zM13.564 19.565v-5.315h-6.75v-4.5h6.75v-5.314l7.564 7.572z",
                                    HE.path' $ HA.d "M0.814 2.625h1.5v18.75h-1.5v-18.75z"
                              ]
                        ]
                  ]
            _ ->
                  HE.div (HA.class' "suggestion") $ HE.div_ $ HE.img $ HA.src "/client/media/logo.png"
      where toInfoSpan includeSepator =
                  case _ of
                        Just s ->
                              [HE.span_ $ s <> " "] <>
                              (if includeSepator then
                                    [HE.span (HA.class' "smaller") "• "]
                               else [])
                        _ -> [HE.createEmptyElement "span"]

            maybeLanguages =
                  case _ of
                        "" -> Nothing
                        l -> Just ("speaks " <> l)

            toTagSpan tag = HE.span (HA.class' "tag") tag
