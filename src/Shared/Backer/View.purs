module Shared.Backer.View where

import Prelude

import Environment (backerCSSHash)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Path as SPT
import Shared.Types (ContentType(..))

view :: Html Unit
view = HE.div (HA.class' "backer") [
      HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href <<< SPT.pathery CSS $ "backer." <> backerCSSHash],
      HE.strong_ "MelanChat depends on people like you to keep running",
      HE.p_ "Above all, MelanChat is a project to enable great conversations and human connection. For that reason, we don't show ads, charge for subscriptions, sell bs comestics or anything that would stand in the way of being the best place ever to chat.",
      HE.p_ "At the same time, running an app has costs. Paying for servers, domains, marketing campaings, art, developer time, etc quickly becomes expensive -- specially for a passion project like MelanChat that is not tied to any big corporation or VC money.",
      HE.strong_ "Make an one time donation via PayPal",
      HE.form [HA.action "https://www.paypal.com/donate", HA.method "post", HA.target "_top"] [
            HE.input [HA.type' "hidden", HA.name "business", HA.value "RAH62A4TZZD7L"],
            HE.input [HA.type' "hidden", HA.name "currency_code", HA.value "USD"],
            HE.input [HA.type' "image", HA.src "https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif", HA.name "submit", HA.title "PayPal - The safer, easier way to pay online!", HA.alt "Donate with PayPal button"],
            HE.img [HA.alt "", HA.src "https://www.paypal.com/en_US/i/scr/pixel.gif", HA.width "1", HA.height "1"]
      ],

      HE.strong_ "Become a backer on Patreon and get sweet rewards",
      HE.a [HA.href "https://www.patreon.com/bePatron?u=41075080", HA.class' "patreon-button"] [
            HE.svg [HA.viewBox "0 0 569 546", HA.class' "svg-patreon"] [
                  HE.g_ [
                        HE.circle' [HA.cx "362.589996", HA.cy "204.589996", HA.r "204.589996"],
                        HE.rect' [HA.height "545.799988", HA.width "100", HA.x "0", HA.y"0"]
                  ]
            ],
            HE.text "Become a Patron"
      ],
      HE.strong_ "Other ways to help",
      HE.p_ [
            HE.text "Not all contributions have to be financial. Spreading the word, inviting new people to try MelanChat out, reporting bugs or undesired behavior are also a lot of help. Likewise, if you have any interest in design or programming, MelanChat is ",
            HE.a (HA.href "https://github.com/melanchat/melanchat") "free and open source software."
      ]
]