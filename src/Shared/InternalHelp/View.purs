module Shared.InternalHelp.View where

import Prelude
import Shared.Types

import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Server.Faq as SH
import Server.Terms as ST
import Server.Privacy  as SP

view :: InternalHelpModel -> Html InternalHelpMessage
view { toggleHelp } =
      HE.div (HA.class' "internal-help") [
            HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/help.css"],
            HE.div (HA.class' "duller center") [
                  HE.text "Learn more about Melanchat",
                  HE.br,
                  HE.text "You may also write us anytime at contact@melan.chat"
            ],
            HE.div (HA.class' "help") $ HE.div_ [
                  HE.div (HA.class' "center") [
                        HE.span [HA.class' {"place-link" : true, "selected": toggleHelp == FAQ }, HA.onClick $ ToggleHelpSection FAQ] "FAQ",
                        HE.span (HA.class' "separator duller")  "•",
                        HE.span [HA.class' {"place-link" : true, "selected": toggleHelp == Terms }, HA.onClick $ ToggleHelpSection Terms] "Terms and conditions",
                        HE.span (HA.class' "separator duller")  "•",
                        HE.span [HA.class' {"place-link" : true, "selected": toggleHelp == Privacy }, HA.onClick $ ToggleHelpSection Privacy] "Privacy policy"
                  ],
                  HE.div (HA.class' {"hidden": toggleHelp /= FAQ}) SH.faq,
                  HE.div (HA.class' {"hidden": toggleHelp /= Terms}) ST.terms,
                  HE.div (HA.class' {"hidden": toggleHelp /= Privacy}) SP.privacy
            ]
      ]
