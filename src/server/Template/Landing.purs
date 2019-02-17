module Template.Landing where

import Hedwig as H
import Template as T
import Template(defaultParameters, externalFooter)
import Effect(Effect)
import Prelude(($))

landing :: Effect String
landing = H.render $ T.template defaultParameters { footer = externalFooter }
