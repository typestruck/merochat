module Server.Privacy where

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE

privacy :: forall m. Html m
privacy =
      HE.div (HA.class' "terms") [
            HE.h2 (HA.class' "no-padding") "Introduction",
            HE.p_ """This is how MelanChat ("the Site", "we", "us", "our") handles all user ("you", "your") data.""",
            HE.h2_ "Data Collected",
            HE.p_ "MelanChat collects the following data:",
            HE.ul (HA.class' "bulleted") [
                  HE.li_ "Email and password when you register",
                  HE.li_ "Profile information provided by you (such as display name, your bio, or country/language)",
                  HE.li_ "Chat history between you and other users",
                  HE.li_ "Other activities in the Site (such as Karma points, your chat statistics and achievements)"
            ],
            HE.h2_ "Data collection",
            HE.p_ """You directly provide most of the data by:""",
            HE.ul (HA.class' "bulleted") [
                  HE.li_ "Registering an account or logging in",
                  HE.li_ "Sending chat messages, participating in chat experiments, and other chat related activities",
                  HE.li_ "Interacting with other users' profiles"
            ],
            HE.p_ "Some data is crunched automatically when calculating:",
            HE.ul (HA.class' "bulleted") [
                  HE.li_ "Your Karma score",
                  HE.li_ "Your next chat match",
                  HE.li_ "Chat statistics"
            ],
            HE.h2_ "Data usage",
            HE.p_ """All the data you submit is directly related to the Site services. None of your data is shared with third parties.""",
            HE.h2_ "Data storage",
            HE.p_ "Your data may be stored in databases, caches or your browser. If you decide to terminate your account, all of your data saved in our databases or caches will be deleted. Otherwise your data is kept indefinitely.",
            HE.h2_ "Cookies",
            HE.p_ "MelanChat only uses session cookies. These have the function of keeping you logged in after you register or log in on the Site.",
            HE.div (HA.class' "hide-internal") [
                  HE.h2_ "Contact",
                  HE.p_ [
                        HE.text "You may contact MelanChat about these policies or any other issues at contact@melan.chat. Alternatively, MelanChat entire source code can be viewed at ",
                        HE.a (HA.href "https://github.com/melanchat/melanchat") "https://github.com/melanchat/melanchat"
                  ]
            ]
      ]
