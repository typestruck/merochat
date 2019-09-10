module Server.IM.Template where

import Prelude
import Shared.Types

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Server.Template (defaultParameters)
import Server.Template as ST
import Server.User as SU
import Shared.IM.Model as SIM
import Shared.IM.View as SIV

template :: User -> Effect String
template user = do
        contents <- ST.template $ defaultParameters {
                content = content,
                javascript = javascript,
                css = css
        }
        F.preMount (QuerySelector "#im") {
                view: SIV.view,
                init: IMModel { user: SU.toIMUser user }
        }
        where   javascript = [
                        HE.script' [HA.type' "text/javascript", HA.src "/client/javascript/im.bundle.js"]
                ]
                css = [
                        HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/im.css"]
                ]
                content = [
                          HE.div' [HA.id "im", HA.class' "im"]
                ]