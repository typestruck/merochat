module Server.Terms where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Routes (routes)

terms ∷ ∀ m. Html m
terms =
      HE.div [HA.class' "terms"]
            [ HE.h2 [HA.class' "no-padding"] [HE.text "Introduction and agreement"]
            , HE.p_ [HE.text """These terms and conditions are the entire agreement between MeroChat ("the Site", "we", "our", "us") and the user of the Site ("you", "your")."""]
            , HE.p_ [HE.text "By using the Site in any capacity, you accept these conditions and agree to comply with the most recent version of this agreement. We reserve the right to update this agreement at any time without prior notice. Your continued use of the Site implies your acceptance of the agreement as updated."]
            , HE.div [HA.class' "hide-internal"]
                    [ HE.h2_ [HE.text "Privacy policy"]
                    , HE.p_
                            [ HE.text "You can view the privacy policy at "
                            , HE.a [HA.href $ routes.help {} <> "#privacy"] [HE.text "https://mero.chat/privacy."]
                            ]
                    ]
            , HE.h2_ [HE.text "Limitations and liability"]
            , HE.p_ [HE.text """The Site is provided "as is" without any warranty. MeroChat nor any of its workers will be liable to any loss or damage related to the use, or inability to use, the Site."""]
            , HE.p_ [HE.text "You are responsible for keeping your account information (such as password or email) and personal data confidential."]
            , HE.h2_ [HE.text "Intellectual property rights"]
            , HE.p_
                    [ HE.text "MeroChat is free (as in freedom) software. You can view the source code license at "
                    , HE.a [ HA.href "https://github.com/typestruck/merochat/blob/master/LICENSE", HA.target "_blank" ] [HE.text "https://github.com/typestruck/merochat/LICENSE."]
                    ]
            , HE.p_ [HE.text """The branding including, but not limited to, the logo, name ("MeroChat") and description ("Friendly Random Chat") may not be copied or otherwise reproduced without our written permission."""]
            , HE.p_ [HE.text "You may not post copyrighted material on the Site."]
            , HE.h2_ [HE.text "Eligibility"]
            , HE.p_ [HE.text "You must be 18 years of age or older to register on the Site."]
            , HE.h2_ [HE.text "Restrictions"]
            , HE.p_ [HE.text "You may not use the Site to"]
            , HE.ul [HA.class' "bulleted"]
                    [ HE.li_ [HE.text "harrass, bully or target groups or individuals"]
                    , HE.li_ [HE.text "promote hate speech or discrimination"]
                    , HE.li_ [HE.text "engage in fraudelent or criminal behavior"]
                    , HE.li_ [HE.text "engage in sexual activities, or send pornographic material"]
                    , HE.li_ [HE.text "distribute viruses or malicious software"]
                    ]
            , HE.p_ [HE.text "Additionally, you may not"]
            , HE.ul [HA.class' "bulleted"]
                    [ HE.li_ [HE.text "spam or use automated tools or scripts to engage with the Site"]
                    , HE.li_ [HE.text "try and bypass the Site's security measures"]
                    , HE.li_ [HE.text "collect or harvest other users' data"]
                    , HE.li_ [HE.text "sell or promote services or products within the Site"]
                    ]
            , HE.h2_ [HE.text "Termination"]
            , HE.p_ [HE.text "We reserve the right to delete, suspend or limit your account at any time for any reason without prior notice."]
            , HE.div [HA.class' "hide-internal"]
                    [ HE.h2_ [HE.text "Contact"]
                    , HE.p_ [HE.text "You may contact MeroChat about these terms or any other issues by sending an email to contact"]
                    ]
            ]
