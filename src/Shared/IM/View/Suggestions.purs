module Shared.IM.View.Suggestions where

import Prelude
import Shared.Types

import Data.Array ((!!))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA

suggestions :: IMModel -> Html IMMessage
suggestions { contacts, suggesting, chatting, suggestions }
      | DM.isJust chatting =
            HE.div (HA.class' "side-suggestions-container") [
                  HE.div [HA.class'"side-suggestion back-side-suggestion", HA.onClick ResumeSuggesting, HA.title "Browse chat suggestions again"] [
                        HE.svg [HA.class' "svg-back-suggestions", HA.viewBox "0 0 30 30"][
                              HE.path' [HA.d "M30 13.125H7.18125L17.6625 2.64375L15 0L0 15L15 30L17.6437 27.3563L7.18125 16.875H30V13.125Z"]
                        ],
                        HE.span (HA.class' "back-suggestions") "Back to chat suggestions"
                  ]
            ]
      | otherwise =
            case suggs of
                  Just {avatar, name } | not $ DA.null contacts -> HE.div (HA.class' "side-suggestions-container") [
                        HE.div [HA.class'"side-suggestion", HA.title "Your chat suggestions"] [
                              HE.div (HA.class' "avatar-contact-list-div faded") [
                                    HE.img [HA.class' $ "avatar-contact-list" <> SA.avatarColorClass (map (_ - 1) suggesting), HA.src $ SA.avatarForRecipient (map (_ - 1) suggesting) avatar]
                              ],
                              HE.div (HA.class' "avatar-contact-list-div margin-less-z") [
                                    HE.img [HA.class' $ "avatar-contact-list" <> SA.avatarColorClass suggesting, HA.src $ SA.avatarForRecipient suggesting avatar]
                              ],
                              HE.div (HA.class' "avatar-contact-list-div margin-less faded") [
                                    HE.img [HA.class' $ "avatar-contact-list" <> SA.avatarColorClass (map (_ + 1) suggesting), HA.src $ SA.avatarForRecipient (map (_ + 1) suggesting) avatar]
                              ],

                              HE.div [HA.class' "contact-profile"] [
                                    HE.span (HA.class' "contact-name") name
                              ]
                        ]
                  ]
                  _ -> HE.div' (HA.class'"side-suggestions-container" )

      where suggs = do
                  index <- suggesting
                  suggestions !! index
