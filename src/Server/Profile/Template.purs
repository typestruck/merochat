module Server.Profile.Template where

import Prelude
import Shared.Profile.Types

import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.Profile.View as SPV

template :: ProfileUser -> Effect String
template user = do
        F.preMount (QuerySelector ".profile-edition") {
                view: \model' ->
                        HE.div_ [
                                javascript,
                                SPV.view model'
                        ],
                init: user
        }
        where   javascript =  HE.script' [HA.type' "text/javascript", HA.src "/client/javascript/profile.bundle.js"]
