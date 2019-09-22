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
import Shared.IM.Model as SIM
import Shared.IM.Model (model)
import Shared.IM.View as SIV

template :: Array IMUser -> IMUser -> Effect String
template suggestions user = do
        parameters <- ST.extendParameters $ defaultParameters {
                javascript = javascript,
                css = css
        }
        F.preMount (QuerySelector "#im") {
                view: \model' -> ST.templateWith $ parameters { content = [SIV.view model'] },
                init: model suggestions user
        }
        where   javascript = [
                        HE.script' [HA.type' "text/javascript", HA.src "/client/javascript/im.bundle.js"]
                ]
                css = [
                        HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/im.css"]
                ]