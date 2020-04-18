module Server.Profile.Template where

import Prelude
import Shared.Profile.Types

import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Server.Template (defaultParameters)
import Server.Template as ST
import Shared.Profile.View as SPV

template :: ProfileUser -> Effect String
template (ProfileUser {}) = do
        parameters <- ST.extendParameters $ defaultParameters {
                javascript = javascript,
                css = css
        }
        F.preMount (QuerySelector ".im") {
                view: \model' -> ST.templateWith $ parameters { content = [SPV.view model'] },
                init: ProfileUser {
                }
        }
        where   javascript = [ HE.script' [HA.type' "text/javascript", HA.src "/client/javascript/profile.bundle.js"] ]
                css = [
                        HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/im.css"]
                ]