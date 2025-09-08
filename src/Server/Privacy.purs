module Server.Privacy where

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE

privacy ∷ ∀ m. Html m
privacy =
      HE.div [HA.class' "terms"]
            [ HE.h2 [HA.class' "no-padding"] [HE.text "Introduction"]
            , HE.p_ [HE.text """This is how MeroChat ("the Site", "we", "us", "our") handles all user ("you", "your") data."""]
            , HE.h2_ [HE.text "Data Collected"]
            , HE.p_ [HE.text "MeroChat collects the following data:"]
            , HE.ul [HA.class' "bulleted"]
                    [ HE.li_ [HE.text "Email and password when you register"]
                    , HE.li_ [HE.text "Profile information provided by you (such as display name, your bio, or country/language)"]
                    , HE.li_ [HE.text "Chat history between you and other users"]
                    , HE.li_ [HE.text "Other activities in the Site (such as Karma points, your chat statistics and achievements)"]
                    ]
            , HE.h2_ [HE.text "Data collection"]
            , HE.p_ [HE.text """You directly provide most of the data by:"""]
            , HE.ul [HA.class' "bulleted"]
                    [ HE.li_ [HE.text "Registering an account or logging in"]
                    , HE.li_ [HE.text "Sending chat messages, participating in chat experiments, and other chat related activities"]
                    , HE.li_ [HE.text "Interacting with other users' profiles"]
                    ]
            , HE.p_ [HE.text "Some data is crunched automatically when calculating:"]
            , HE.ul [HA.class' "bulleted"]
                    [ HE.li_ [HE.text "Your Karma score"]
                    , HE.li_ [HE.text "Your next chat match"]
                    , HE.li_ [HE.text "Chat statistics"]
                    ]
            , HE.h2_ [HE.text "Data usage"]
            , HE.p_ [HE.text """All the data you submit is directly related to the Site services. None of your data is shared with third parties."""]
            , HE.h2_ [HE.text "Data storage"]
            , HE.p_ [HE.text "Your data may be stored in databases, caches or your browser. If you decide to terminate your account, all of your data saved in our databases or caches will be deleted. Otherwise your data is kept indefinitely."]
            , HE.h2_ [HE.text "Cookies"]
            , HE.p_ [HE.text "MeroChat only uses session cookies. These have the function of keeping you logged in after you register or log in on the Site."]
            , HE.div [HA.class' "hide-internal"]
                    [ HE.h2_ [HE.text "Contact"]
                    , HE.p_
                            [ HE.text "You may contact MeroChat about these policies or any other issues by sending an email to contact. Alternatively, MeroChat entire source code can be viewed at "
                            , HE.a [ HA.href "https://github.com/typestruck/merochat", HA.target "_blank" ] [HE.text "https://github.com/typestruck/merochat"]
                            ]
                    ]
            ]
