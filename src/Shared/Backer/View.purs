module Shared.Backer.View where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Backer.Types (BackerMessage, BackerModel)

view ∷ BackerModel -> Html BackerMessage
view model = HE.div (HA.class' {"backer" : true, hidden: not model.visible})
      [ HE.h3 (HA.class' "initial bottom") "MeroChat depends on people like you to keep running"
      , HE.p_ "We will never show ads, charge for subscriptions, sell cosmetics or anything that would stand in the way of being the best place ever to chat"
      , HE.p_ "At the same time: apps have costs. Servers, domains, marketing campaigns, design and development time, etc. quickly become expensive. All the more for a passion project like MeroChat that is not tied to any big corporations or VC money"
      , HE.p_ "So why not pitch in if you can? All proceeds go straight into making this place even better :)"
      , HE.h3_ "Make an one time donation via PayPal"
      , HE.form [ HA.action "https://www.paypal.com/donate", HA.method "post", HA.target "_blank" ]
              [ HE.input [ HA.type' "hidden", HA.name "business", HA.value "RAH62A4TZZD7L" ]
              , HE.input [ HA.type' "hidden", HA.name "currency_code", HA.value "USD" ]
              , HE.input [ HA.type' "image", HA.src "https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif", HA.name "submit", HA.title "PayPal - The safer, easier way to pay online!", HA.alt "Donate with PayPal button" ]
              , HE.img [ HA.alt "", HA.src "https://www.paypal.com/en_US/i/scr/pixel.gif", HA.width "1", HA.height "1" ]
              ]
      , HE.h3_ "Become a backer on Liberapay"
      , HE.a [ HA.href "https://liberapay.com/merochat/donate", HA.target "_blank" ] [ HE.img [ HA.alt "Donate using Liberapay", HA.src "https://liberapay.com/assets/widgets/donate.svg" ] ]
      , HE.h3_ "Become a backer on Patreon"
      , HE.a [ HA.href "https://www.patreon.com/bePatron?u=41075080", HA.target "_blank", HA.class' "patreon-button" ]
              [ HE.svg [ HA.viewBox "0 0 569 546", HA.class' "svg-patreon" ]
                      [ HE.g_
                              [ HE.circle' [ HA.cx "362.589996", HA.cy "204.589996", HA.r "204.589996" ]
                              , HE.rect' [ HA.height "545.799988", HA.width "100", HA.x "0", HA.y "0" ]
                              ]
                      ]
              , HE.text "Become a Patron"
              ]
      , HE.h3 ((HA.class' "bottom")) "Other ways to help"
      , HE.p_
              [ HE.text "Not all contributions have to be financial. Spreading the word, inviting new people to try MeroChat out, reporting bugs or undesired behavior is also a lot of help" ]
      , HE.p_
              [ HE.text "Likewise, if you have any interest in design or programming, MeroChat is "
              , HE.a [ HA.href "https://github.com/typestruck/merochat", HA.target "_blank" ] "free and open source software"
              ]
      ]