module Server.Terms where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Routes (routes)

terms :: forall m.  Html m
terms  =
      HE.div (HA.class' "terms") [
            HE.h2 (HA.class' "no-padding") "Introduction and agreement",
            HE.p_ """These terms and conditions are the entire agreement between MelanChat ("the Site", "we", "our", "us") and the user of the Site ("you", "your").""",
            HE.p_ "By using the Site in any capacity, you accept these conditions and agree to comply with the most recent version of this agreement. We reserve the right to update this agreement at any time without prior notice. Your continued use of the Site implies your acceptance of the agreement as updated.",
            HE.div (HA.class' "hide-internal") [
                  HE.h2_ "Privacy policy",
                  HE.p_ [
                        HE.text "You can view the privacy policy at ",
                        HE.a (HA.href $ routes.help {} <> "#privacy") "https://melan.chat/privacy."
                  ]
            ],
            HE.h2_ "Limitations and liability",
            HE.p_ """The Site is provided "as is" without any warranty. MelanChat nor any of its workers will be liable to any loss or damage related to the use, or inability to use, the Site.""",
            HE.p_ "You are responsible for keeping your account information (such as password or email) and personal data confidential.",
            HE.h2_ "Intellectual property rights",
            HE.p_ [
                  HE.text "MelanChat is free (as in freedom) software. You can view the source code license at ",
                  HE.a (HA.href "https://github.com/melanchat/melanchat/blob/master/LICENSE") "https://github.com/melanchat/melanchat/LICENSE."
            ],
            HE.p_ """The branding including, but not limited to, the logo, name ("MelanChat") and description ("Friendly Random Chat") may not be copied or otherwise reproduced without our written permission.""",
            HE.p_ "You may not post copyrighted material on the Site.",
            HE.h2_ "Eligibility",
            HE.p_ "You must be 13 years of age or older to register on the Site.",
            HE.h2_ "Restrictions",
            HE.p_ "You may not use the Site to",
            HE.ul (HA.class' "bulleted") [
                  HE.li_ "harrass, bully or target groups or individuals",
                  HE.li_ "promote hate speech or discrimination",
                  HE.li_ "engage in fraudelent or criminal behavior",
                  HE.li_ "engage in sexual activities, or send pornographic material",
                  HE.li_ "distribute viruses or malicious software"
            ],
            HE.p_ "Additionally, you may not",
            HE.ul (HA.class' "bulleted") [
                  HE.li_ "spam or use automated tools or scripts to engage with the Site",
                  HE.li_ "try and bypass the Site's security measures",
                  HE.li_ "collect or harvest other users' data",
                  HE.li_ "sell or promote services or products within the Site"
            ],
            HE.h2_ "Termination",
            HE.p_ "We reserve the right to delete, suspend or limit your account at any time for any reason without prior notice.",
            HE.div (HA.class' "hide-internal") [
                  HE.h2_ "Contact",
                  HE.p_ "You may contact MelanChat about these terms or any other issues at contact@melan.chat"
            ]
      ]
