module Shared.Backer.View where

import Prelude

import Shared.Modal (Modal(..))
import Shared.Modal as SM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Icon as SI
import Shared.Backer.Types (BackerMessage(..), BackerModel)
import Shared.Extra as SE

view ∷ BackerModel → Html BackerMessage
view model = HE.div [ HA.id "backer" ]
      [ HE.div [ HA.class' { "backer": true, hidden: not model.visible } ]
              [ SM.header "Support us" $ ToggleVisibility HideModal
              , SE.h3 [ HA.class' "bottom initial" ] [ HE.text "MeroChat depends on people like you to keep running" ]
              , SE.p_ [ HE.text "We will never show ads, charge for subscriptions, sell cosmetics or anything that would stand in the way of being the best place ever to chat" ]
              , SE.p_ [ HE.text "At the same time: apps have costs. Servers, domains, marketing campaigns, design and development time, etc. quickly become expensive. All the more for a passion project like MeroChat that is not tied to any big corporations or VC money" ]
              , SE.p_ [ HE.text "So why not pitch in if you can? All proceeds go straight into making this place even better :)" ]
              , SE.h3_ [ HE.text "Make an one time donation via PayPal" ]
              , SE.a [ HA.href "https://www.paypal.com/donate/?business=RAH62A4TZZD7L&currency_code=USD", SE.externalLink "https://www.paypal.com/donate/?business=RAH62A4TZZD7L&currency_code=USD", HA.target "_blank", HA.class' "donation-button" ] [ HE.text "Donate with PayPal" ]
              , SE.h3_ [ HE.text "Become a backer on Liberapay" ]
              , SE.a [ HA.href "https://liberapay.com/merochat/donate", SE.externalLink "https://liberapay.com/merochat/donate", HA.target "_blank", HA.class' "donation-button" ] [ HE.text "Donate with Liberapay" ]
              , SE.h3_ [ HE.text "Become a backer on Patreon" ]
              , SE.a [ HA.href "https://www.patreon.com/bePatron?u=41075080", SE.externalLink "https://www.patreon.com/bePatron?u=41075080", HA.target "_blank", HA.class' "patreon-button inline-control" ]
                      [ SI.svg [ HA.viewBox "0 0 569 546", HA.class' "svg-patreon" ]
                              [ HE.g_
                                      [ HE.circle' [ HA.cx "362.589996", HA.cy "204.589996", HA.r "204.589996" ]
                                      , HE.rect' [ HA.height "545.799988", HA.width "100", HA.x "0", HA.y "0" ]
                                      ]
                              ]
                      , HE.text "Become a Patron"
                      ]
              , SE.h3 [ HA.class' "bottom" ] [ HE.text "Other ways to help" ]
              , SE.p [ HA.class' "lynx-inline" ]
                      [ HE.text "Not all contributions have to be financial. Spreading the word, inviting new people to try MeroChat out, reporting bugs or undesired behavior is also a lot of help" ]
              , SE.p [ HA.class' "lynx-inline" ]
                      [ HE.text "Likewise, if you have any interest in design or programming, MeroChat is "
                      , SE.a [ HA.href "https://github.com/typestruck/merochat", SE.externalLink "https://github.com/typestruck/merochat", HA.target "_blank" ] [ HE.text "free and open source software" ]
                      ]
              ]
      ]
