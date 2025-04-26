module Shared.Im.View.SuggestionCall where

import Prelude
import Shared.Im.Types

import Data.Array ((!!))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.Im.Svg as SIS

suggestionCall ∷ ImModel → Html ImMessage
suggestionCall { contacts, suggesting, chatting, suggestions, toggleModal }
      | DM.isJust chatting =
              HE.div (HA.class' { "side-suggestions-container": true, highlighted: toggleModal == Tutorial BackSuggestions })
                    [ HE.div [ HA.class' "side-suggestion back-side-suggestion", HA.onClick ResumeSuggesting, HA.title "Browse chat suggestions again" ]
                            [ SIS.arrow [ HA.class' "svg-back-suggestions" ]
                            , HE.span (HA.class' "back-suggestions") "Back to chat suggestions"
                            ]
                    ]
      | otherwise =
              case suggs of
                    Just { avatar, name } | not $ DA.null contacts → HE.div (HA.class' "side-suggestions-container")
                          [ HE.div [ HA.class' "side-suggestion" ]
                                  [ HE.div [ HA.class' "avatar-contact-list-div faded", HA.onClick $ SpecialRequest PreviousSuggestion, HA.title "Move to this chat suggestion" ]
                                          [ SA.avatar [ HA.class' $ "avatar-contact-list", HA.src $ SA.fromAvatar $ getAvatar (suggesting - 1) ]
                                          ]
                                  , HE.div [ HA.class' "avatar-contact-list-div margin-less-z", HA.onClick FocusCurrentSuggestion, HA.title "Move to this chat suggestion" ]
                                          [ SA.avatar [ HA.class' "avatar-contact-list", HA.src $ SA.fromAvatar avatar ]
                                          ]
                                  , HE.div [ HA.class' "avatar-contact-list-div margin-less faded", HA.onClick $ SpecialRequest NextSuggestion, HA.title "Move to this chat suggestion" ]
                                          [ SA.avatar [ HA.class' $ "avatar-contact-list", HA.src $ SA.fromAvatar  $ getAvatar (suggesting + 1) ]
                                          ]
                                  , HE.div [ HA.class' "contact-profile", HA.title "Your chat suggestions" ]
                                          [ HE.span (HA.class' "contact-name") name
                                          ]
                                  ]
                          ]
                    _ → HE.div' (HA.class' "side-suggestions-container")

              where
              suggs = suggestions !! suggesting

              getAvatar index = do
                    user ← suggestions !! index
                    user.avatar