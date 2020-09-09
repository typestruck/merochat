--REFACTOR: this also shows contacts profiles not only suggestions as the name implies
module Shared.IM.View.Suggestion where

import Prelude
import Shared.Types

import Control.Alt ((<|>))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Semigroup.Foldable as DF
import Data.String as DS
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Flame.Renderer.Hook as FRH
import Shared.Avatar as SA
import Shared.Markdown as SM

profile :: IMModel -> Maybe IMUser -> Html IMMessage
profile { suggesting, chatting } =
      case _ of
            Just ({ id, name, avatar, age, karma, headline, gender, country, languages, tags, description }) ->
                  HE.div (HA.class' suggestionClasses) [
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

                        HE.img [HA.class' $ "avatar-profile " <> SA.avatarColorClass (chatting <|> suggesting), HA.src $ SA.avatarForRecipient (chatting <|> suggesting) avatar],
                        HE.h1_ name,
                        HE.span (HA.class' "headline") headline,
                        HE.div (HA.class' "profile-karma") [
                              -- HE.div_ [
                              --       HE.span [HA.class' "greyer"] "Last seen",
                              --       HE.text " Yesterday"
                              -- ],
                              HE.div_ [
                                    HE.text $ show karma,
                                    HE.span [HA.class' "greyer"] " karma"
                              ]
                        ],

                        HE.div (HA.class' "profile-asl") [
                              HE.div_ [
                                    toSpan $ map show age,
                                    greyer (DM.isNothing age || DM.isNothing gender) ", ",
                                    toSpan gender
                              ],
                              HE.div_ [
                                    greyer (DM.isNothing country) "from ",
                                    toSpan country
                              ],
                              HE.div_ ([
                                    greyer (DA.null languages) "speaks "
                              ] <> (DA.intercalate [greyer false ", "] $ map (DA.singleton <<< spanWith) languages))
                        ],

                        HE.div (HA.class' "profile-tags") $ map toTagSpan tags,

                        greyer false "About",
                              -- HE.div_ $ HE.button [HA.class' "action-button", HA.onClick $ BlockUser id] "Block"

                        HE.div' [HA.class' "description-message", FRH.atPostpatch (SM.toHTML description)]

                  ]

            _ ->
                  HE.div (HA.class' "suggestion") $ HE.div_ $ HE.img $ HA.src "/client/media/logo.png"
      where suggestionClasses = "suggestion " <> if DM.isJust chatting then "old" else "new"
            toSpan =
                  case _ of
                        Just s -> spanWith s
                        _ -> HE.createEmptyElement "span"
            spanWith = HE.span (HA.class' "")
            toTagSpan tag = HE.span (HA.class' "tag") tag

            greyer hidden t = HE.span (HA.class' { "greyer" : true, "hidden": hidden }) t